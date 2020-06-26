{ config, lib, options, pkgs, ... }:

with lib;
(let
  cfg = config.modules.desktop.tools.fcitx;
  fcitx-with-plugins =
    (with pkgs; fcitx.override { plugins = with fcitx-engines; [ mozc ]; });
  gtkEnabled = config.modules.desktop.config.gtk.enable;
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
in {
  options.modules.desktop.tools.fcitx = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    extraClassicUIConfig = mkOption {
      type = types.lines;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    modules.desktop.backends.dbus = {
      enable = mkForce true;
      packages = with pkgs; [ fcitx-with-plugins ];
    };

    my.packages = with pkgs;
      ([ fcitx-with-plugins fcitx-configtool ]
        ++ optionals gtkEnabled [ gtk2-immodule-cache gtk3-immodule-cache ]);
    my.home.xdg.configFile = {
      "fcitx/config".source = <config/fcitx/config>;
      "fcitx/conf/fcitx-classic-ui.config".text = ''
        ${readFile <config/fcitx/conf/fcitx-classic-ui.config>}

        ${cfg.extraClassicUIConfig}
      '';
      "gtk-2.0/immodules.cache".source =
        mkIf gtkEnabled "${gtk2-immodule-cache}/etc/gtk-2.0/immodules.cache";
      "gtk-3.0/immodules.cache".source =
        mkIf gtkEnabled "${gtk3-immodule-cache}/etc/gtk-3.0/immodules.cache";
    };
    my.env = {
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
      GTK_IM_MODULE = mkIf gtkEnabled "fcitx";
    };
    my.xsession.init = ''
      ${fcitx-with-plugins}/bin/fcitx &
    '';
  };
})
