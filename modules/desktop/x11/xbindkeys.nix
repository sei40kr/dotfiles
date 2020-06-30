{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.x11.xbindkeys.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.x11.xbindkeys.enable {
    modules.desktop.x11.xsession.init = ''
      ${pkgs.xbindkeys}/bin/xbindkeys
    '';

    my.packages = with pkgs; [ xbindkeys ];
    my.home.home.file.".xbindkeysrc" = {
      source = <config/xbindkeys/xbindkeysrc>;
      onChange = "$DRY_RUN_CMD ${pkgs.xbindkeys}/bin/xbindkeys -p";
    };
  };
}
