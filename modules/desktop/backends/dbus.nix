{ config, lib, options, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.backends.dbus;
in {
  options.modules.desktop.backends.dbus = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    packages = mkOption {
      type = with types; listOf package;
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    services.dbus = {
      enable = true;
      packages = cfg.packages;
    };
  };
}
