{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.input.fcitx.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.input.fcitx.enable {
    my = {
      packages = with pkgs; [ fcitx fcitx-configtool fcitx-engines.mozc ];

      env = {
        GTK_IM_MODULE = "fcitx";
        QT_IM_MODULE = "fcitx";
        XMODIFIERS = "@im=fcitx";
      };
      xsession.init = ''
        ${pkgs.fcitx}/bin/fcitx &
      '';
    };

    xdg.configFile."fcitx/config".source = <config/fcitx/config>;
  };
}
