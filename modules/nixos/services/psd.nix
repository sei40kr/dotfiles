{ config, lib, ... }:

with lib;
with lib.my;
(let cfg = config.modules.services.psd;
in {
  options.modules.services.psd.enable = mkBoolOpt false;

  config.services.psd.enable = cfg.enable;
})
