{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.dconf;
in {
  options.modules.desktop.dconf = with types; {
    enable = mkBoolOpt false;

    settings = mkOpt attrs { };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.dconf = {
      inherit (cfg) settings;

      enable = true;
    };
  };
}
