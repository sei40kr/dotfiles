{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.jupyter;
  package = pkgs.python.withPackages
    (ps: with ps; [ jupyter ipython matplotlib numpy pandas ]);
in {
  options.modules.dev.jupyter = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = [ package ]; };
}
