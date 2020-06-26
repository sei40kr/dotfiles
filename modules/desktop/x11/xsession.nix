{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.x11.xsession;
  xsessionWrapper = pkgs.writeScript "xsession-wrapper" ''
    ${cfg.sessionCommands}
  '';
in {
  options.modules.desktop.x11.xsession = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    autoRepeatDelay = mkOption {
      type = types.int;
      default = 150;
    };

    autoRepeatInterval = mkOption {
      type = types.int;
      default = 30;
    };

    profile = mkOption {
      type = types.lines;
      default = "";
    };

    init = mkOption {
      type = types.lines;
      default = "";
    };

    variablesImportedIntoSystemdSession = mkOption {
      type = with types; listOf (strMatching "[a-zA-Z_][a-zA-Z0-9_]*");
      default = [
        "DBUS_SESSION_BUS_ADDRESS"
        "DISPLAY"
        "SSH_AUTH_SOCK"
        "XAUTHORITY"
        "XDG_DATA_DIRS"
        "XDG_RUNTIME_DIR"
        "XDG_SESSION_ID"
      ];
    };
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;

    my.home.xsession = {
      enable = true;
      profileExtra = cfg.profile;
      initExtra = ''
        /run/current-system/systemd/bin/systemctl --user import-environment ${
          escapeShellArgs cfg.variablesImportedIntoSystemdSession
        }

        ${pkgs.xorg.xset}/bin/xset r rate ${toString cfg.autoRepeatDelay} ${
          toString cfg.autoRepeatInterval
        }

        ${cfg.init}
      '';
      # Manually import environment variables into user systemd session
      importedVariables = [ ];
    };
  };
}
