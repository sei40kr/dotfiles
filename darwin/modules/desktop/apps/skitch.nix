{ config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.skitch;
in
{
  options.modules.desktop.apps.skitch = {
    enable = mkEnableOption "Skitch";
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      masApps = { Skitch = 425955336; };
    };
  };
}
