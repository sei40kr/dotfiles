{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  aiCfg = config.modules.ai;
  cfg = aiCfg.codex;
in
{
  options.modules.ai.codex = {
    enable = mkEnableOption "Codex CLI";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ unstable.codex ];
  };
}
