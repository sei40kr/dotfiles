{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell.kaggle;
  package = pkgs.kaggle.overrideAttrs (_: { doCheck = false; });
in {
  options.modules.shell.kaggle = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = [ package ]; };
}
