{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.evince;
in {
  options.modules.desktop.media.evince.enable = mkBoolOpt false;

  config.programs.evince.enable = cfg.enable;
}
