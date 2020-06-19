{ config, lib, options, pkgs, ... }:

with lib;
let
  fcitx-with-plugins =
    (with pkgs; fcitx.override { plugins = with fcitx-engines; [ mozc ]; });
  gtk2-immodule-cache = pkgs.stdenv.mkDerivation {
    preferLocalBuild = true;
    allowSubstitutes = false;
    name = "gtk2-immodule.cache";
    buildInputs = with pkgs; [ gtk2 fcitx-with-plugins ];
    buildCommand = ''
      mkdir -p $out/etc/gtk-2.0
      GTK_PATH=${fcitx-with-plugins}/lib/gtk-2.0 gtk-query-immodules-2.0 >$out/etc/gtk-2.0/immodules.cache
    '';
  };
  gtk3-immodule-cache = pkgs.stdenv.mkDerivation {
    preferLocalBuild = true;
    allowSubstitutes = false;
    name = "gtk3-immodule.cache";
    buildInputs = with pkgs; [ gtk3 fcitx-with-plugins ];
    buildCommand = ''
      mkdir -p $out/etc/gtk-3.0
      GTK_PATH=${fcitx-with-plugins}/lib/gtk-3.0 gtk-query-immodules-3.0 >$out/etc/gtk-3.0/immodules.cache
    '';
  };
  fontsEnabled = config.modules.desktop.fonts.enable;
  gtkEnabled = config.modules.desktop.gtk.enable;
in {
  options.modules.desktop.i18n.japanese.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.i18n.japanese.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ fcitx-with-plugins ];
    };

    my.packages = with pkgs;
      ([ fcitx-with-plugins fcitx-configtool ]
        ++ optionals fontsEnabled [ noto-fonts-cjk ]
        ++ optionals gtkEnabled [ gtk2-immodule-cache gtk3-immodule-cache ]);
    my.env = {
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
      GTK_IM_MODULE = mkIf gtkEnabled "fcitx";
    };
    my.home.xdg.configFile = {
      "fcitx/config".source = <config/fcitx/config>;
      "fontconfig/conf.d/70-noto-cjk.conf".source =
        mkIf fontsEnabled <config/fontconfig/70-noto-cjk.conf>;
      "gtk-2.0/immodules.cache".source =
        mkIf gtkEnabled "${gtk2-immodule-cache}/etc/gtk-2.0/immodules.cache";
      "gtk-3.0/immodules.cache".source =
        mkIf gtkEnabled "${gtk3-immodule-cache}/etc/gtk-3.0/immodules.cache";
    };
    my.xsession.init = ''
      ${fcitx-with-plugins}/bin/fcitx &
    '';
  };
}
