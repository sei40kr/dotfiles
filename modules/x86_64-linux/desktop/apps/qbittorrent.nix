{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.qbittorrent;
in {
  options.modules.desktop.apps.qbittorrent = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ qbittorrent ]; };
}
