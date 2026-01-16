{
  config,
  inputs,
  perSystem,
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
  imports = [
    inputs.self.homeModules.ai-shared
  ];

  options.modules.ai.agent-browser = {
    enable = mkEnableOption "agent-browser";
    package = mkPackageOption perSystem.llm-agents-nix "agent-browser" { };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    modules.ai.skillPaths = [ "${cfg.package}/etc/agent-browser/skills" ];
  };
}
