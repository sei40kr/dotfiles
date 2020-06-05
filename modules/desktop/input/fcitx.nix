{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.input.fcitx.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.input.fcitx.enable {
    home.packages = with pkgs; [ fcitx fcitx-configtool fcitx-engines.mozc ];

    xdg.configFile."fcitx/config".source = <config/fcitx/config>;
  };
}
