{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  inherit (config.i18n.inputMethod) package;
  cfg = config.modules.desktop.fcitx5;
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

    environment.etc = mkIf config.modules.desktop.sway.enable {
      "sway/config.d/startup/fcitx5.conf".text = ''
        exec ${package}/bin/fcitx5
      '';
      "sway/config.d/bindings/fcitx5.conf".text = ''
        bindsym Control+space exec ${pkgs.procps}/bin/pkill --signal SIGUSR2 waybar
      '';
    };
  };
}
