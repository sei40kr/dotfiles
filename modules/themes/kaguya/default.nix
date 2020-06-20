{ config, lib, options, pkgs, ... }:

with lib;
let
  accentColor = "#9a2223";
  rofiEnabled = config.modules.desktop.apps.rofi.enable;
  japaneseEnabled = config.modules.desktop.i18n.japanese.enable;
in {
  options.modules.themes.kaguya.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.themes.kaguya.enable {
    my.home.gtk = {
      iconTheme = {
        package = pkgs.gnome3.adwaita-icon-theme;
        name = "Adwaita";
      };
      theme = null;
      gtk3 = {
        extraConfig.gtk-application-prefer-dark-theme = true;
        extraCss = "${readFile ./gtk/gtk-3.0/gtk.css}";
      };
    };

    modules.desktop.xmonad.themeConfig = ./xmonad/src/Lib/Theme.hs;

    modules.desktop.apps.polybar.themeConfig = ./polybar/theme.conf;

    modules.desktop.apps.rofi.theme = "kaguya";
    my.home.xdg.configFile."rofi/kaguya.rasi".source =
      mkIf rofiEnabled ./rofi/theme.rasi;

    modules.desktop.browsers.qutebrowser.themeConfig = ./qutebrowser/theme.py;

    modules.desktop.tools.fcitx.extraClassicUIConfig = ''
      # Skin Name
      SkinType=dark
    '';

    modules.desktop.tools.randomBackground.imageDirectory =
      toString ./random-background;
  };
}
