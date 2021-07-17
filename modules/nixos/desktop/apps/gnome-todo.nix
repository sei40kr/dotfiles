{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.gnome-todo;
in {
  options.modules.desktop.apps.gnome-todo.enable = mkBoolOpt false;

  config = mkIf cfg.enable { user.packages = with pkgs; [ gnome.gnome-todo ]; };
}
