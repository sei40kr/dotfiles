{ config, lib, ... }:

let
  inherit (lib) mkIf types;
  inherit (lib.my)
    mkBoolOpt
    mkOpt
    ;
  inherit (types) attrsOf anything;
  cfg = config.modules.desktop.dconf;
in
{
  options.modules.desktop.dconf = {
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
