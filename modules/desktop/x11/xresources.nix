{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.x11.xresources.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.x11.xresources.enable {
    modules.desktop.x11.xsession.loadXDefaults = mkForce true;

    my.home.home.file.".Xresources" = {
      source = <config/xresources/Xresources>;
      onChange = ''
        if [[ -v DISPLAY ]]; then
          $DRY_RUN_CMD ${pkgs.xorg.xrdb}/bin/xrdb -merge ~/.Xresources
        fi
      '';
    };
  };
}
