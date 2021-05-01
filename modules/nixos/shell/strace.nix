{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.strace;
in {
  options.modules.shell.strace = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ strace ]; };
}
