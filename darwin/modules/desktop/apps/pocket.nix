{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.pocket;
in
{
  options.modules.desktop.apps.pocket = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    homebrew = {
    enable=true;
    masApps={Pocket = 568494494;};
    };
  };
}
