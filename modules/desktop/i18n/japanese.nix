{ config, lib, options, pkgs, ... }:

with lib;
let fontconfigEnabled = config.modules.desktop.config.fontconfig.enable;
in {
  options.modules.desktop.i18n.japanese.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.i18n.japanese.enable {
    modules.desktop.tools.fcitx.enable = mkForce true;

    my.packages = with pkgs; optionals fontconfigEnabled [ noto-fonts-cjk ];
    my.home.xdg.configFile."fontconfig/conf.d/70-noto-cjk.conf".source =
      mkIf fontconfigEnabled <config/fontconfig/70-noto-cjk.conf>;
  };
}
