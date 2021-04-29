{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.fcitx;
  package =
    pkgs.fcitx5-with-addons.override { addons = with pkgs; [ fcitx5-mozc ]; };
  gtk3_cache = pkgs.runCommand "gtk3-immodule.cache" {
    preferLocalBuild = true;
    allowSubstitutes = false;
    buildInputs = [ package pkgs.gtk3 ];
  } ''
    mkdir -p "''${out}/lib/gtk-3.0/3.0.0"
    GTK_PATH='${package}/lib/gtk-3.0' gtk-query-immodules-3.0 >"''${out}/lib/gtk-3.0/3.0.0/immodules.cache"
  '';
in {
  options.modules.desktop.fcitx = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ package ];

    home-manager.users.${config.user.name}.systemd.user.services.fcitx5 = {
      Unit = {
        After = [ "graphical-session.target" ];
        Description = "Flexible Input Method Framework";
        Documentation = [ "man:fcitx5(1)" ];
      };
      Service.ExecStart =
        "${package}/bin/fcitx5 --disable clipboard,kimpanel,quickphrase,spell,unicode,xim";
      Install.WantedBy = [ "graphical-session.target" ];
    };
    services.dbus = {
      enable = true;
      packages = [ package ];
    };
    modules.desktop.env = {
      GTK_IM_MODULE = "fcitx";
      GTK_IM_MODULE_FILE = "${gtk3_cache}/lib/gtk-3.0/3.0.0/immodules.cache";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
    };
  };
}
