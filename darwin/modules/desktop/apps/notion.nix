{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.notion;
in
{
  options.modules.desktop.apps.notion = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      casks = [ "notion" ];
    };
  };
}
