{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.pomodoro;
in {
  options.modules.desktop.apps.pomodoro.enable = mkBoolOpt false;

  config = mkIf cfg.enable { user.packages = with pkgs; [ gnome.pomodoro ]; };
}
