{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.tools.xbindkeys.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.tools.xbindkeys.enable {
    my.packages = with pkgs; [ xbindkeys ];
    my.home.home.file.".xbindkeysrc" = {
      source = <config/xbindkeys/xbindkeysrc>;
      onChange = "${pkgs.xbindkeys}/bin/xbindkeys -p";
    };
    my.xsession.init = ''
      xbindkeys
    '';
  };
}
