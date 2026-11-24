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

    # Input sources: macOS's ABC for English and azooKey for Japanese, in place
    # of Kotoeri. Writing the list directly skips the System Settings approval
    # TISEnableInputSource needs for a third-party IME.
    targets.darwin.defaults."com.apple.HIToolbox".AppleEnabledInputSources = [
      {
        InputSourceKind = "Keyboard Layout";
        "KeyboardLayout ID" = 252;
        "KeyboardLayout Name" = "ABC";
      }
      {
        "Bundle ID" = "dev.ensan.inputmethod.azooKeyMac";
        InputSourceKind = "Keyboard Input Method";
      }
      {
        "Bundle ID" = "dev.ensan.inputmethod.azooKeyMac";
        "Input Mode" = "com.apple.inputmethod.Japanese";
        InputSourceKind = "Input Mode";
      }
      {
        "Bundle ID" = "com.apple.CharacterPaletteIM";
        InputSourceKind = "Non Keyboard Input Method";
      }
      {
        "Bundle ID" = "com.apple.PressAndHold";
        InputSourceKind = "Non Keyboard Input Method";
      }
    ];

    # Text Input Sources don't register a bundle symlinked into the store, so
    # copy it, and only when the store path changes since the IME restarts.
    # It has to be in place before the input source list above names it.
    home.activation.installAzooKey =
      lib.hm.dag.entryBetween [ "setDarwinDefaults" ] [ "writeBoundary" ]
        ''
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
