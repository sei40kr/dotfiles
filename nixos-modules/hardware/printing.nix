{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.printing;
in {
  options.modules.hardware.printing = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services = {
      printing = {
        enable = true;
        drivers = with pkgs; [ gutenprint ];
      };
      avahi = {
        enable = true;
        nssmdns = true;
      };
    };
  };
}
