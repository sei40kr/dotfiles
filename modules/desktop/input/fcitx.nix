{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.input.fcitx.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.input.fcitx.enable {
    home.packages = with pkgs; [ fcitx fcitx-configtool fcitx-engines.mozc ];

    xdg.configFile."fcitx/config".source = <config/fcitx/config>;

    # Initialize Fcitx in X Session
    xsession = {
      profileExtra = ''
        export GTK_IM_MODULE=fcitx
        export QT_IM_MODULE=fcitx
        export XMODIFIERS='@im=fcitx'
      '';
      initExtra = ''
        ${pkgs.fcitx}/bin/fcitx &
      '';
    };
  };
}
