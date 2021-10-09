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

      # cf. https://github.com/NixOS/nixpkgs/issues/129442
      NIX_PROFILES =
        concatStringsSep " " (reverseList config.environment.profiles);
    };
    home.configFile."fcitx5/config".source = "${configDir}/fcitx5/config";
  };
}
