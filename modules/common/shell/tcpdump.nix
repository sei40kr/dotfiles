{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.tcpdump;
in {
  options.modules.shell.tcpdump = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ tcpdump ]; };
}
