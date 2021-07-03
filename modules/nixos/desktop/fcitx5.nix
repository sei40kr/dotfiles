{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.fcitx5;
in {
  options.modules.desktop.fcitx5.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-mozc ];
    };

    home.configFile."fcitx5/config".source = "${configDir}/fcitx5/config";
  } // mkIf config.modules.desktop.gnome.enable {
    user.packages = with pkgs; [ gnomeExtensions.kimpanel ];

    modules.desktop.gnome.enabledExtensions = [ "kimpanel@kde.org" ];
  };
}
