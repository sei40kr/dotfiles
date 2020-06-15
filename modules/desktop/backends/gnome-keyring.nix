{ config, lib, options, pkgs, ... }:

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
    my.home.services.gnome-keyring = {
      enable = true;
      components = cfg.components;
    };
  };
}
