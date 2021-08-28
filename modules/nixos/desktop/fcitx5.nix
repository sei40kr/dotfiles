{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  inherit (config.i18n.inputMethod) package;
  desktopCfg = config.modules.desktop;
  cfg = desktopCfg.fcitx5;
in {
  options.modules.desktop.fcitx5 = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-mozc ];
    };
    environment.sessionVariables = {
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
    };
    home.configFile."fcitx5/config".source = "${configDir}/fcitx5/config";

    environment.etc."sway/config.d/startup/fcitx5.conf".text =
      mkIf desktopCfg.sway.enable ''
        exec ${package}/bin/fcitx5
      '';
  };
}
