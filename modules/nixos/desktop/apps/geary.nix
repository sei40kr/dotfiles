{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.geary;
in {
  options.modules.desktop.apps.geary.enable = mkBoolOpt false;

  config.programs.geary.enable = cfg.enable;
}
