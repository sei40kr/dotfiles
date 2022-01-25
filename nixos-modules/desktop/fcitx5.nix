{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.i18n.inputMethod) package;
  cfg = config.modules.desktop.fcitx5;

  fcitx5-with-addons =
    pkgs.fcitx5-with-addons.override { addons = with pkgs; [ fcitx5-mozc ]; };

  gtk2_cache = pkgs.runCommandLocal "gtk2-immodule.cache" {
    buildInputs = [ fcitx5-with-addons pkgs.gtk2 ];
  } ''
    mkdir -p $out/etc/gtk-2.0
    GTK_PATH=${fcitx5-with-addons}/lib/gtk-2.0 gtk-query-immodules-2.0 >$out/etc/gtk-2.0/immodules.cache
  '';

  gtk3_cache = pkgs.runCommandLocal "gtk3-immodule.cache" {
    buildInputs = [ fcitx5-with-addons pkgs.gtk3 ];
  } ''
    mkdir -p $out/etc/gtk-3.0
    GTK_PATH=${fcitx5-with-addons}/lib/gtk-3.0 gtk-query-immodules-3.0 >$out/etc/gtk-3.0/immodules.cache
  '';
in {
  options.modules.desktop.fcitx5 = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ fcitx5-with-addons gtk2_cache gtk3_cache ];

    systemd.user.services.fcitx5-daemon = {
      enable = true;
      serviceConfig = {
        Type = "simple";
        ExecStart = "${fcitx5-with-addons}/bin/fcitx5 -r";
        Restart = "on-failure";
      };
      wantedBy = [ "graphical-session.target" ];
    };

    environment.variables = {
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
    };
  };
}
