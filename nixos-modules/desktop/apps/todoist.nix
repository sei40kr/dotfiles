{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.todoist;
in {
  options.modules.desktop.apps.todoist = { enable = mkBoolOpt false; };

  config =
    mkIf cfg.enable { user.packages = with pkgs; [ todoist-electron ]; };
}
