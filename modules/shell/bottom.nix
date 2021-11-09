{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.bottom;
in {
  options.modules.shell.bottom = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ bottom ];
  };
}
