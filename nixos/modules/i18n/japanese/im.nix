{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.i18n.japanese;

  package = pkgs.fcitx5-with-addons.override {
    addons = with pkgs; [ fcitx5-mozc ];
  };
  gtk2-cache = pkgs.runCommand "gtk2-immodule.cache"
    {
      preferLocalBuild = true;
      allowSubstitutes = false;
      buildInputs = [ pkgs.gtk2 package ];
    }
    ''
      mkdir -p $out/etc/gtk-2.0
      GTK_PATH=${package}/lib/gtk-2.0 gtk-query-immodules-2.0 >$out/etc/gtk-2.0/immodules.cache
    '';

  gtk3-cache = pkgs.runCommand "gtk3-immodule.cache"
    {
      preferLocalBuild = true;
      allowSubstitutes = false;
      buildInputs = [ pkgs.gtk3 package ];
    }
    ''
      mkdir -p $out/etc/gtk-3.0
      GTK_PATH=${package}/lib/gtk-3.0 gtk-query-immodules-3.0 >$out/etc/gtk-3.0/immodules.cache
    '';
in
{
  config = mkIf (cfg.enable && config.modules.desktop.enable) {
    user.packages = [ package gtk2-cache gtk3-cache ];

    environment.variables = {
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
      QT_PLUGIN_PATH = [ "${package}/${pkgs.qt6.qtbase.qtPluginPrefix}" ];
    };

    systemd.user.services.fcitx5-daemon = {
      description = "Fcitx5 input method editor";
      documentation = [ "https://fcitx-im.org" ];
      partOf = [ "autostart.target" ];
      wantedBy = [ "autostart.target" ];
      aliases = [ "input-method.service" ];
      serviceConfig = {
        ExecStart = "${package}/bin/fcitx5";
      };
    };
  };
}
