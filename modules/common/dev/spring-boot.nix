{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.spring-boot;
in {
  options.modules.dev.spring-boot = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ spring-boot ]; };
}
