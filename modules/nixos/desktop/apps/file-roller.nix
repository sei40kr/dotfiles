{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.file-roller;
in {
  options.modules.desktop.apps.file-roller.enable = mkBoolOpt false;

  config.programs.file-roller.enable = cfg.enable;
}
