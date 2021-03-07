{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.online-judge-tools;
in {
  options.modules.shell.online-judge-tools = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs.my; [
      python3Packages.online-judge-tools
      python3Packages.online-judge-verify-helper
    ];
  };
}
