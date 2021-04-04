{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.hugo;
in {
  options.modules.shell.hugo = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ hugo ]; };
}
