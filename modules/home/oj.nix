{
  config,
  lib,
  pkgs,
  perSystem,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.shell.oj;
in
{
  options.modules.shell.oj = {
    enable = mkEnableOption "Online Judge tools";
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.online-judge-tools
      pkgs.online-judge-template-generator
      perSystem.self.online-judge-verify-helper
    ];
  };
}
