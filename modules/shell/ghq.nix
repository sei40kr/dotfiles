{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.ghq;
in {
  options.modules.shell.ghq = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ ghq ]; };
}
