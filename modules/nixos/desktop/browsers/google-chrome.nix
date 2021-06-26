{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.google-chrome;
  package = pkgs.google-chrome.override { commandLineArgs = "--disable-gpu"; };
in {
  options.modules.desktop.browsers.google-chrome.enable = mkBoolOpt false;

  config = mkIf cfg.enable { user.packages = [ package ]; };
}
