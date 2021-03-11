{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.fcitx;
  package = pkgs.unstable.fcitx5-with-addons.override {
    addons = with pkgs; [ unstable.fcitx5-mozc ];
  };
  gtk2_cache = pkgs.runCommand "gtk2-immodule.cache" {
    preferLocalBuild = true;
    allowSubstitutes = false;
    buildInputs = [ package pkgs.unstable.gtk2 ];
  } ''
    mkdir -p "''${out}/etc/gtk-2.0"
    GTK_PATH='${package}/lib/gtk-2.0' gtk-query-immodules-2.0 >"''${out}/etc/gtk-2.0/immodules.cache"
  '';
  gtk3_cache = pkgs.runCommand "gtk3-immodule.cache" {
    preferLocalBuild = true;
    allowSubstitutes = false;
    buildInputs = [ package pkgs.unstable.gtk3 ];
  } ''
    mkdir -p "''${out}/etc/gtk-3.0"
    GTK_PATH='${package}/lib/gtk-3.0' gtk-query-immodules-3.0 >"''${out}/etc/gtk-3.0/immodules.cache"
  '';
in {
  options.modules.desktop.fcitx = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ package pkgs.unstable.fcitx5-configtool ]
      ++ optionals config.modules.desktop.gtk.enable [ gtk2_cache gtk3_cache ];
    modules.desktop.env = {
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      SDL_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
    };
    home.configFile."fcitx5/config".source = "${configDir}/fcitx5/config";
    services.dbus = {
      enable = true;
      packages = [ package ];
    };
  };
}
