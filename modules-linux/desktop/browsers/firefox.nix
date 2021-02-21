{ config, home-manager, lib, ... }:

with lib; {
  options.modules.desktop.browsers.firefox.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.browsers.firefox.enable {
    modules.desktop.config.gtk.enable = mkForce true;

    home-manager.users.${config.user.name}.programs.firefox.enable = true;
  };
}
