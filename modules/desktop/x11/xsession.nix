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

    startDBusSession = mkOption {
      type = types.bool;
      default = false;
    };

    updateDBusEnvironment = mkOption {
      type = types.bool;
      default = false;
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
        # Start a new D-Bus session
        ${optionalString cfg.startDBusSession ''
          if [[ -z "$DBUS_SESSION_BUS_ADDRESS" ]]; then
            ${pkgs.systemd}/bin/systemctl --user start dbus.socket
            export "$(${pkgs.systemd}/bin/systemctl --user show-environment | grep '^DBUS_SESSION_BUS_ADDRESS')"
          fi
        ''}

        # Redirect the output to the systemd journal
        if [[ -z "$__DID_SYSTEMD_CAT" ]]; then
          export __DID_SYSTEMD_CAT=1
          ${pkgs.systemd}/bin/systemd-cat -t xsession "$0" "$@"
        fi

        # Import environment variables to the user systemd session
        ${pkgs.systemd}/bin/systemctl --user import-environment ${
          escapeShellArgs cfg.variablesImportedIntoSystemdSession
        }

        ${pkgs.xorg.xset}/bin/xset r rate ${toString cfg.autoRepeatDelay} ${
          toString cfg.autoRepeatInterval
        }

        # Update D-Bus activation environment
        ${optionalString cfg.updateDBusEnvironment
        "${pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all"}

        ${cfg.init}
      '';
      # Manually import environment variables into user systemd session
      importedVariables = [ ];
    };
  };
}
