{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.apps.neofetch;
in
{
  options.modules.shell.apps.neofetch = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ neofetch ];

    environment.etc."neofetch/config.conf".source = ../../../config/neofetch/config.conf;
  };
}
