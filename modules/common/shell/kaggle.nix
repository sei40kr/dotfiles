{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.kaggle;
in {
  options.modules.shell.kaggle = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ kaggle ]; };
}
