{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.ristretto;
in {
  options.modules.desktop.media.ristretto = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ xfce.ristretto ]; };
}
