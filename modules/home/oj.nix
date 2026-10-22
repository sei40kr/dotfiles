{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.shell.oj;
in
{
  options.modules.shell.oj = {
    enable = mkEnableOption "online-judge-tools";
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.online-judge-tools
      pkgs.online-judge-template-generator
      pkgs.online-judge-verify-helper
    ];
  };
}
