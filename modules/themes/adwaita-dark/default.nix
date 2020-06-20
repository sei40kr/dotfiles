{ config, lib, options, pkgs, ... }:

with lib;
let
  accentColor = "#9a2223";
  rofiEnabled = config.modules.desktop.apps.rofi.enable;
  japaneseEnabled = config.modules.desktop.i18n.japanese.enable;
in {
  options.modules.themes.adwaitaDark.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.themes.adwaitaDark.enable {
    my.home.gtk = {
      iconTheme = {
        package = pkgs.gnome3.adwaita-icon-theme;
        name = "Adwaita";
      };
      theme = null;
      gtk3 = {
        extraConfig.gtk-application-prefer-dark-theme = true;
        extraCss = ''
          /* base background color of selections */
          @define-color theme_selected_bg_color ${accentColor};
          /* text/foreground color of selections */
          @define-color theme_selected_fg_color #ffffff;

          *:selected { background-color: @theme_selected_bg_color; }
          *.view:selected { background-color: @theme_selected_bg_color; }

          selection { background-color: @theme_selected_bg_color; }

          textview selection { background-color: @theme_selected_bg_color; }

          switch:checked { background-color: @theme_selected_bg_color; }

          menu menuitem:hover, .menu menuitem:hover {
            background-color: @theme_selected_bg_color;
          }
        '';
      };
    };

    modules.desktop.xmonad.themeConfig = ./xmonad/src/Lib/Theme.hs;

    modules.desktop.apps.rofi.theme = "adwaita-dark";
    my.home.xdg.configFile."rofi/adwaita-dark.rasi".source =
      mkIf rofiEnabled ./rofi/theme.rasi;

    modules.desktop.browsers.qutebrowser.themeConfig = ./qutebrowser/theme.py;

    modules.desktop.tools.randomBackground.imageDirectory =
      toString ./random-background;

    my.home.xdg.configFile."fcitx/conf/fcitx-classic-ui.config".source =
      mkIf japaneseEnabled ./fcitx/conf/fcitx-classic-ui.config;
  };
}
