{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.lang.go;
in
{
  options.modules.dev.lang.go = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ go gopls gore ];
  };
}
