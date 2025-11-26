{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    ;
  cfg = config.modules.dev.tools.difftastic;
in
{
  options.modules.dev.tools.difftastic = {
    enable = mkEnableOption "Difftastic";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ difftastic ];
  };
}
