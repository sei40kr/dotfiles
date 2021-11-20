{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.online-judge-tools;
in {
  options.modules.shell.online-judge-tools = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      my.online-judge-tools
      my.online-judge-template-generator
      my.online-judge-verify-helper
    ];
  };
}
