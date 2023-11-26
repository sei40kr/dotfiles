{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.just-focus;
in
{
  options.modules.desktop.apps.just-focus = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      masApps = { JustFocus = 1142151959; };
    };
  };
}
