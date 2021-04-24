{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.file-roller;
in {
  options.modules.desktop.apps.file-roller = { enable = mkBoolOpt false; };

  config =
    mkIf cfg.enable { user.packages = with pkgs; [ gnome3.file-roller ]; };
}
