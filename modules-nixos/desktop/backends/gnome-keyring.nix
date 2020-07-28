{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.backends.gnomeKeyring;
in {
  options.modules.desktop.backends.gnomeKeyring = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    components = mkOption {
      type = with types; listOf (enum [ "pkcs11" "secrets" "ssh" ]);
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    modules.desktop = {
      backends = {
        dbus = {
          enable = mkForce true;
          packages = with pkgs; [ gnome3.gnome-keyring gcr ];
        };
        gsettingsDesktopSchemas = {
          enable = mkForce true;
          packages = with pkgs; [ gnome3.gnome-keyring gcr ];
        };
      };
      x11.xsession.init = ''
        eval "$(${config.security.wrapperDir}/gnome-keyring-daemon --start --components=${
          concatStringsSep "," cfg.components
        })"
        export SSH_AUTH_SOCK
      '';
    };

    # NOTE Avoid "insufficient process capabilities".
    #      See https://unix.stackexchange.com/questions/112030/gnome-keyring-daemon-insufficient-process-capabilities-unsecure-memory-might-g
    security.wrappers.gnome-keyring-daemon = {
      source = "${pkgs.gnome3.gnome-keyring}/bin/gnome-keyring-daemon";
      capabilities = "cap_ipc_lock=ep";
    };

    my.packages = with pkgs; [ gnome3.gnome-keyring gcr ];
  };
}
