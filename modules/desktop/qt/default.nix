{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.qt;

  isKvantum = cfg.kvantum.theme != null;
in
{
  options.modules.desktop.qt = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      qgnomeplatform
      qgnomeplatform-qt6
      (mkIf (!isKvantum) adwaita-qt)
      (mkIf (!isKvantum) adwaita-qt6)
    ];

    environment.variables = {
      QT_QPA_PLATFORMTHEME = "gnome";
      # TODO: support other Qt styles
      QT_STYLE_OVERRIDE = mkIf (!isKvantum) "adwaita";
    };
  };
}
