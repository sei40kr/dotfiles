{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.google-chrome;
in {
  options.modules.desktop.browsers.google-chrome = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable { user.packages = with pkgs; [ google-chrome ]; };
}
