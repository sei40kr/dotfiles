{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.backends.dconf.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.backends.dconf.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ dconf ];
    };

    my.home.dconf.enable = true;

    my.env.GIO_EXTRA_MODULES = [ "${pkgs.dconf.lib}/lib/gio/modules" ];
  };
}
