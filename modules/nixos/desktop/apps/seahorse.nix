{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.seahorse;
in {
  options.modules.desktop.apps.seahorse.enable = mkBoolOpt false;

  config.programs.seahorse.enable = cfg.enable;
}
