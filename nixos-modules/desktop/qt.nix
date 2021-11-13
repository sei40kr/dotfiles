{ config, lib, ... }:

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
  };
}
