{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.shell.oj;
in
{
  options.modules.shell.oj = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      online-judge-tools
      online-judge-template-generator
      my.online-judge-verify-helper
    ];
  };
}
