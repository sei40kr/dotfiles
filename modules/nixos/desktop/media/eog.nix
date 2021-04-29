{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.eog;
  package = pkgs.gnome3.eog;
in {
  options.modules.desktop.media.eog = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = [ package ]; };
}
