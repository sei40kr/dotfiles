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
    modules.desktop.x11.xsession = {
      startDBusSession = mkForce true;
      updateDBusEnvironment = mkForce true;
    };

    services.dbus = {
      enable = true;
      packages = cfg.packages;
    };
  };
}
