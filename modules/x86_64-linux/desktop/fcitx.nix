{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.fcitx;
in {
  options.modules.desktop.fcitx = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable (let
    package = with pkgs;
      fcitx.override { plugins = with fcitx-engines; [ mozc ]; };
  in {
    user.packages = with pkgs; [ package fcitx-configtool ];
    home.configFile = {
      "fcitx/config".source = "${configDir}/fcitx/config";
      "fcitx/conf/fcitx-classic-ui.config".source =
        "${configDir}/fcitx/conf/fcitx-classic-ui.config";
    };
    services.dbus = {
      enable = true;
      packages = [ package ];
    };
  });
}
