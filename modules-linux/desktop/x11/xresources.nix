{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.desktop.x11.xresources.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.x11.xresources.enable {
    modules.desktop.x11.xsession.loadXDefaults = mkForce true;

    home.file.".Xresources" = {
      source = "${configDir}/xresources/Xresources";
      onChange = ''
        if [[ -v DISPLAY ]]; then
          $DRY_RUN_CMD ${pkgs.xorg.xrdb}/bin/xrdb -merge ~/.Xresources
        fi
      '';
    };
  };
}
