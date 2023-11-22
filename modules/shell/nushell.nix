{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell.nushell;
in
{
  options.modules.shell.nushell = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.nushell = {
      enable = true;
      extraConfig = ''
        $env.config = {
          show_banner: false
        }
      '';
    };
  };
}
