{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.notion;
in {
  options.modules.desktop.apps.notion.enable = mkBoolOpt false;

  config =
    mkIf cfg.enable { user.packages = with pkgs; [ my.notion-app-enhanced ]; };
}
