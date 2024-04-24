{ config, lib, ... }:

let
inherit (lib) mkEnableOption mkIf;
cfg = config.modules.desktop.apps.notion-calendar;
in
{
  options.modules.desktop.apps.notion-calendar = {
    enable = mkEnableOption "Notion Calendar";
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      casks = [ "notion-calendar" ];
    };
  };
}
