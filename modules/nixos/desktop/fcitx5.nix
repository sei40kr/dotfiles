{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.fcitx5;
in {
  options.modules.desktop.fcitx5.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-mozc ];
    };
    environment.sessionVariables = {
      NIX_PROFILES =
        "${concatStringsSep " " (reverseList config.environment.profiles)}";
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
    };

    home.configFile."fcitx5/config".source = "${configDir}/fcitx5/config";

    modules.desktop.gnome.extensions = {
      packages = with pkgs; [ gnomeExtensions.kimpanel ];
      names = [ "kimpanel@kde.org" ];
    };
  };
}
