{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.i18n.japanese.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.i18n.japanese.enable {
    modules.desktop.tools.fcitx.enable = mkForce true;

    my.packages = with pkgs;
      optionals config.modules.desktop.fonts.enable [ noto-fonts-cjk ];
    my.home.xdg.configFile."fontconfig/conf.d/70-noto-cjk.conf".source =
      mkIf config.modules.desktop.config.fontconfig.enable
      <config/fontconfig/70-noto-cjk.conf>;
  };
}
