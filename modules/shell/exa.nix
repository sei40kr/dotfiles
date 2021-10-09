{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.exa;
in {
  options.modules.shell.exa = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ exa ];
    modules.shell.aliases = {
      ls = "exa -F";
      la = "exa -laFh";
      tree = "exa -T";
    };
  };
}
