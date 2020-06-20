{ config, lib, options, pkgs, ... }:

with lib;
let fontsEnabled = config.modules.desktop.fonts.enable;
in {
  options.modules.desktop.i18n.japanese.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.i18n.japanese.enable {
    modules.desktop.tools.fcitx.enable = mkForce true;

    my.packages = with pkgs; optionals fontsEnabled [ noto-fonts-cjk ];
    my.home.xdg.configFile."fontconfig/conf.d/70-noto-cjk.conf".source =
      mkIf fontsEnabled <config/fontconfig/70-noto-cjk.conf>;
  };
}
