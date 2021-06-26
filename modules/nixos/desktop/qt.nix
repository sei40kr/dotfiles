{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.qt;
in {
  options.modules.desktop.qt = with types; {
    enable = mkBoolOpt false;
    theme = {
      platformTheme = mkOpt (enum [ "gnome" "gtk" ]) "gtk";
      style = mkOpt (nullOr attrs) null;
    };
  };

  config.home-manager.users.${config.user.name}.qt = {
    inherit (cfg) enable;
    inherit (cfg.theme) platformTheme style;
  };
}
