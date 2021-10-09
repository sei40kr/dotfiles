{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.eog;
in {
  options.modules.desktop.media.eog = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ gnome.eog ]; };
}
