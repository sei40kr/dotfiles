{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.aws-cli;
  package = pkgs.awscli;
in {
  options.modules.dev.aws-cli = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = [ package ]; };
}
