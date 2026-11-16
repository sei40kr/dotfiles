{
  lib,
  config,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.i18n.azookey;
  app = "${perSystem.self.azookey-desktop}/Library/Input Methods/azooKeyMac.app";
in
{
  options.modules.i18n.azookey = {
    enable = mkEnableOption "azooKey Desktop, the macOS counterpart of Hazkey";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.hostPlatform.isDarwin;
        message = "azooKey Desktop is only available on macOS; use modules.i18n.hazkey on Linux.";
      }
    ];

    # Text Input Sources don't register a bundle symlinked into the store, so
    # copy it, and only when the store path changes since the IME restarts.
    home.activation.installAzooKey = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      dest="$HOME/Library/Input Methods/azooKeyMac.app"
      stamp="${config.xdg.stateHome}/azookey/source"
      if [[ ! -d "$dest" || "$(readlink "$stamp")" != ${lib.escapeShellArg app} ]]; then
        run mkdir -p "$HOME/Library/Input Methods" "$(dirname "$stamp")"
        if [[ -d "$dest" ]]; then
          run chmod -R u+w "$dest"
          run rm -rf "$dest"
        fi
        run cp -R ${lib.escapeShellArg app} "$dest"
        run chmod -R u+w "$dest"
        run ln -sfn ${lib.escapeShellArg app} "$stamp"
        # The running IME keeps the old binary until macOS relaunches it.
        run /usr/bin/pkill -x azooKeyMac || true
      fi
    '';
  };
}
