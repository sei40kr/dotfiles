{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.oj;
in {
  options.modules.shell.oj = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      my.online-judge-tools
      my.online-judge-template-generator
      my.online-judge-verify-helper
    ];
  };
}
