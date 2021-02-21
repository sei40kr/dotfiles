{ config, home-manager, lib, pkgs, ... }:

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

    home-manager.users.${config.user.name}.dconf.enable = true;
  };
}
