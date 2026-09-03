#!/usr/bin/env bash
#
# Repair Nix after a macOS update breaks it.
#
# sudo is invoked exactly once: diagnostics run unprivileged, then a single
# root phase does all privileged work.
#
# Usage:
#   ./repair-nix-macos.sh

set -euo pipefail

readonly NIX_PROFILE=/nix/var/nix/profiles/default
readonly DAEMON_PLIST=/Library/LaunchDaemons/org.nixos.nix-daemon.plist
readonly STORE_PLIST=/Library/LaunchDaemons/org.nixos.darwin-store.plist
readonly SHELL_SNIPPET="# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix"

log() { printf '\033[1;34m==>\033[0m %s\n' "$*" >&2; }
warn() { printf '\033[1;33mwarning:\033[0m %s\n' "$*" >&2; }
die() { printf '\033[1;31merror:\033[0m %s\n' "$*" >&2; exit 1; }

volume_exists() { diskutil list | grep -q 'Nix Store'; }
volume_mounted() { mount | grep -q ' on /nix '; }
store_intact() { [ -x "$NIX_PROFILE/bin/nix" ]; }
nixbld_exists() { dscl . -read /Groups/nixbld PrimaryGroupID > /dev/null 2>&1; }
daemon_running() { pgrep -qx nix-daemon; }
shell_hook_ok() { grep -q 'nix-daemon.sh' "$1" 2> /dev/null; }

diagnose() {
  log "Diagnosing Nix installation ..."
  local ok=y
  volume_exists || { warn "APFS volume 'Nix Store' not found"; ok=n; }
  volume_mounted || { warn "/nix is not mounted"; ok=n; }
  volume_mounted && ! store_intact && { warn "$NIX_PROFILE/bin/nix missing"; ok=n; }
  nixbld_exists || { warn "nixbld group missing"; ok=n; }
  [ -f "$DAEMON_PLIST" ] || { warn "nix-daemon LaunchDaemon plist missing"; ok=n; }
  daemon_running || { warn "nix-daemon is not running"; ok=n; }
  for f in /etc/bashrc /etc/zshrc; do
    shell_hook_ok "$f" || { warn "Nix hook missing from $f"; ok=n; }
  done
  [ "$ok" = y ]
}

root_mount_volume() {
  grep -q '^nix' /etc/synthetic.conf 2> /dev/null || printf 'nix\n' >> /etc/synthetic.conf
  if [ ! -d /nix ]; then
    /System/Library/Filesystems/apfs.fs/Contents/Resources/apfs.util -t 2> /dev/null ||
      /System/Library/Filesystems/apfs.fs/Contents/Resources/apfs.util -B 2> /dev/null || true
  fi
  [ -d /nix ] || die "/nix mount point could not be created (a reboot may be required)"

  if ! volume_mounted; then
    log "Mounting 'Nix Store' volume ..."
    # Unencrypted first, then FileVault volume via the System keychain passphrase
    diskutil mount -mountPoint /nix 'Nix Store' 2> /dev/null ||
      security find-generic-password -s 'Nix Store' -w /Library/Keychains/System.keychain |
        diskutil apfs unlockVolume 'Nix Store' -mountpoint /nix -stdinpassphrase ||
      die "failed to mount the Nix Store volume"
  fi

  if ! grep -q ' /nix apfs ' /etc/fstab 2> /dev/null; then
    local uuid
    uuid=$(diskutil info 'Nix Store' | awk '/Volume UUID/ { print $3 }')
    [ -n "$uuid" ] &&
      printf 'UUID=%s /nix apfs rw,noauto,nobrowse,suid,owners\n' "$uuid" >> /etc/fstab
  fi
}

root_restore_daemons() {
  if [ ! -f "$STORE_PLIST" ]; then
    log "Restoring darwin-store LaunchDaemon ..."
    cat > "$STORE_PLIST" << 'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>org.nixos.darwin-store</string>
  <key>RunAtLoad</key>
  <true/>
  <key>ProgramArguments</key>
  <array>
    <string>/bin/sh</string>
    <string>-c</string>
    <string>/usr/sbin/diskutil mount -mountPoint /nix 'Nix Store' || /usr/bin/security find-generic-password -s 'Nix Store' -w /Library/Keychains/System.keychain | /usr/sbin/diskutil apfs unlockVolume 'Nix Store' -mountpoint /nix -stdinpassphrase</string>
  </array>
</dict>
</plist>
EOF
    chmod 644 "$STORE_PLIST"
  fi

  if [ ! -f "$DAEMON_PLIST" ]; then
    log "Restoring nix-daemon LaunchDaemon ..."
    cp "$NIX_PROFILE/Library/LaunchDaemons/org.nixos.nix-daemon.plist" "$DAEMON_PLIST"
    chmod 644 "$DAEMON_PLIST"
  fi

  launchctl bootstrap system "$STORE_PLIST" 2> /dev/null || true
  launchctl bootstrap system "$DAEMON_PLIST" 2> /dev/null || true
  launchctl enable system/org.nixos.nix-daemon 2> /dev/null || true
  launchctl kickstart -k system/org.nixos.nix-daemon
}

root_restore_shell_hooks() {
  local f
  for f in /etc/bashrc /etc/zshrc; do
    if ! shell_hook_ok "$f"; then
      log "Restoring Nix hook in $f ..."
      printf '\n%s\n' "$SHELL_SNIPPET" >> "$f"
    fi
  done
}

root_repair() {
  root_mount_volume
  store_intact || die "Nix store is damaged; reinstall Nix with the official installer"
  [ -f /etc/nix/nix.conf ] || {
    log "Recreating minimal /etc/nix/nix.conf ..."
    mkdir -p /etc/nix
    printf 'build-users-group = nixbld\n' > /etc/nix/nix.conf
  }
  root_restore_daemons
  root_restore_shell_hooks
}

verify() {
  log "Verifying ..."
  store_intact || { warn "nix binary not found"; return 1; }
  local i
  for i in 1 2 3 4 5; do
    daemon_running && break
    sleep 1
  done
  daemon_running || { warn "nix-daemon did not start"; return 1; }
  # --store daemon forces a real daemon connection even as root
  local out
  out=$("$NIX_PROFILE/bin/nix" --extra-experimental-features nix-command \
    store info --store daemon 2>&1) ||
    out=$("$NIX_PROFILE/bin/nix" --extra-experimental-features nix-command \
      store ping --store daemon 2>&1) || {
      warn "could not talk to the nix-daemon:"
      printf '%s\n' "$out" >&2
      return 1
    }
  log "Nix is working: $("$NIX_PROFILE/bin/nix" --version)"
  log "Open a NEW shell to pick up the restored /etc/zshrc hook."
}

main() {
  [ "$(uname -s)" = Darwin ] || die "this script is for macOS only"

  # Internal entry point for the single-sudo root phase
  if [ "${1:-}" = --as-root ]; then
    [ "$(id -u)" -eq 0 ] || die "--as-root must run as root"
    root_repair
    return
  fi

  [ "$(id -u)" -ne 0 ] || die "run without sudo; this script calls sudo itself"
  volume_exists && nixbld_exists ||
    die "no repairable Nix installation found; reinstall Nix with the official installer"

  if diagnose; then
    log "Nothing looks broken. Nix appears healthy."
    verify && return
    warn "verification failed despite healthy diagnostics; repairing anyway"
  fi

  local self
  self="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"

  log "sudo will prompt once; keep the Jamf admin window active."
  SECONDS=0
  sudo bash "$self" --as-root
  log "Privileged phase finished in ${SECONDS}s."

  verify
}

main "$@"
