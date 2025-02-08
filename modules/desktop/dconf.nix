{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.dconf;
in
{
  options.modules.desktop.dconf = with types; {
    enable = mkBoolOpt false;

    settings = mkOpt (attrsOf anything) { };
  };

  config = mkIf cfg.enable {
    programs.dconf.enable = true;

    home-manager.users.${config.user.name}.dconf = {
      inherit (cfg) settings;

      enable = true;
    };
  };
}
