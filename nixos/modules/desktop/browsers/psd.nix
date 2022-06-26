{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.psd;
in
{
  options.modules.desktop.browsers.psd = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.psd.enable = true;
  };
}
