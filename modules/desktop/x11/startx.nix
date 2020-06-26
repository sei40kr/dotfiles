{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.x11.startx.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.x11.startx.enable {
    modules.desktop.x11.xsession.enable = mkForce true;

    services.xserver.displayManager.startx.enable = true;

    my.home.home.file.".xinitrc".text = ''
      ./.xsession
    '';
  };
}
