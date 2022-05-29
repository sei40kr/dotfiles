{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.element;

  package =
    if config.modules.desktop.wayland then
      pkgs.element-desktop-wayland
    else
      pkgs.element-desktop;
in
{
  options.modules.desktop.apps.element = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ package ];
  };
}
