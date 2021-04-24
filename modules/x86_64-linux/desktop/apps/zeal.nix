{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.zeal;
in {
  options.modules.desktop.apps.zeal = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ zeal ]; };
}
