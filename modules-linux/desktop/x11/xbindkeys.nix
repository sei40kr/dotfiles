{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.desktop.x11.xbindkeys.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.x11.xbindkeys.enable {
    modules.desktop.x11.xsession.init = ''
      ${pkgs.xbindkeys}/bin/xbindkeys
    '';

    user.packages = with pkgs; [ xbindkeys ];
    home.file.".xbindkeysrc" = {
      source = "${configDir}/xbindkeys/xbindkeysrc";
      onChange = "$DRY_RUN_CMD ${pkgs.xbindkeys}/bin/xbindkeys -p";
    };
  };
}
