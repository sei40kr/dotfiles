{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.fcitx;
  package = with pkgs;
    fcitx.override { plugins = with fcitx-engines; [ mozc ]; };
  gtk2-cache = with pkgs;
    runCommand "gtk2-immodule.cache" {
      preferLocalBuild = true;
      allowSubstitutes = false;
      buildInputs = [ gtk2 package ];
    } ''
      mkdir -p "''${out}/etc/gtk-2.0"
      GTK_PATH='${package}/lib/gtk-2.0' gtk-query-immodules-2.0 >"''${out}/etc/gtk-2.0/immodules.cache"
    '';

  gtk3-cache = with pkgs;
    runCommand "gtk3-immodule.cache" {
      preferLocalBuild = true;
      allowSubstitutes = false;
      buildInputs = [ gtk3 package ];
    } ''
      mkdir -p "''${out}/etc/gtk-3.0"
      GTK_PATH='${package}/lib/gtk-3.0' gtk-query-immodules-3.0 >"''${out}/etc/gtk-3.0/immodules.cache"
    '';
in {
  options.modules.desktop.fcitx = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      ([ package fcitx-configtool ]
        ++ optionals config.modules.desktop.gtk.enable [
          gtk2-cache
          gtk3-cache
        ]);
    modules.desktop.env = {
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
    };
    home.configFile = {
      "fcitx/config".source = "${configDir}/fcitx/config";
      "fcitx/conf/fcitx-classic-ui.config".source =
        "${configDir}/fcitx/conf/fcitx-classic-ui.config";
    };
    home-manager.users.${config.user.name}.systemd.user.services = {
      fcitx = {
        Unit = {
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
          Description = "Flexible Input Method Framework";
          Documentation = "man:fcitx(1)";
          X-Restart-Triggers = let configFile = config.home.configFile;
          in [
            "${configFile."fcitx/config".source}"
            "${configFile."fcitx/conf/fcitx-classic-ui.config".source}"
          ];
        };
        Service = {
          Environment = [
            "GTK_IM_MODULE=fcitx"
            "QT_IM_MODULE=fcitx"
            "XMODIFIERS=@im=fcitx"
          ];
          ExecReload = "${package}/bin/fcitx -r";
          ExecStart = "${package}/bin/fcitx -D";
          Restart = "on-failure";
        };
        Install = { WantedBy = [ "graphical-session.target" ]; };
      };
    };
    services.dbus = {
      enable = true;
      packages = [ package ];
    };
  };
}
