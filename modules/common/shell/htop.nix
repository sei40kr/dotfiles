{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.htop;
in {
  options.modules.shell.htop = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ htop ];
    modules.shell.aliases.top = "htop";
  };
}
