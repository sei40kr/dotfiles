{
  config,
  inputs',
  lib,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkPackageOption
    ;
  cfg = config.modules.ai.agent-browser;
in
{
  options.modules.ai.agent-browser = {
    enable = mkEnableOption "agent-browser";
    package = mkPackageOption inputs'.llm-agents-nix.packages "agent-browser" { };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    modules.ai.skillPaths = [ "${cfg.package}/etc/agent-browser/skills" ];
  };
}
