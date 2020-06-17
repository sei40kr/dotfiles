{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.fonts.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.fonts.enable {
    my.home.fonts.fontconfig.enable = mkForce true;
    my.home.xdg.configFile."fontconfig/conf.d/10-hinting-none.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/10-hinting-none.conf";
    my.home.xdg.configFile."fontconfig/conf.d/10-sub-pixel-rgb.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/10-sub-pixel-rgb.conf";
    my.home.xdg.configFile."fontconfig/conf.d/11-lcdfilter-default.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/11-lcdfilter-default.conf";
    my.home.xdg.configFile."fontconfig/conf.d/66-noto-sans.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/66-noto-sans.conf";
    my.home.xdg.configFile."fontconfig/conf.d/66-noto-serif.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/66-noto-serif.conf";
    my.home.xdg.configFile."fontconfig/conf.d/66-noto-mono.conf".source =
      "${pkgs.fontconfig}/share/fontconfig/conf.avail/66-noto-mono.conf";

    my.packages = with pkgs; [ fontconfig noto-fonts noto-fonts-emoji ];
  };
}
