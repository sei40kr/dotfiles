{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.qt;
in {
  options.modules.desktop.qt = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.qt = {
      enable = true;
      platformTheme = "gtk";
    };
    modules.desktop.env.QT_QPA_PLATFORMTHEME = "gtk3";
  };
}
