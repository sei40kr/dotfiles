{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.i18n.japanese.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.i18n.japanese.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ fcitx ];
    };

    my.packages = with pkgs;
      ([
        (fcitx.override { plugins = with pkgs.fcitx-engines; [ mozc ]; })
        fcitx-configtool
      ] ++ optionals config.modules.desktop.fonts.enable [ noto-fonts-cjk ]);
    my.env = {
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
    };
    my.home.xdg.configFile."fcitx/config".source = <config/fcitx/config>;
    my.home.xdg.configFile."fcitx/conf/fcitx-classic-ui.config".text = ''
      ${readFile <config/fcitx/conf/fcitx-classic-ui.config>}
      # Skin Name
      SkinType=${
        if config.modules.themes.preferDarkTheme then "dark" else "default"
      }
    '';

    # Fontconfig configuration
    my.home.xdg.configFile."fontconfig/conf.d/70-noto-cjk.conf".source =
      "${pkgs.fontconfig_210}/share/fontconfig/conf.avail/70-noto-cjk.conf";
  };
}
