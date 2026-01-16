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
    mapAttrs
    ;
  aiCfg = config.modules.ai;
  cfg = aiCfg.crush;

  convertMcpServer =
    name: server:
    if server.transport == "stdio" then
      {
        inherit (server) command args env;
        type = "stdio";
      }
    else if server.transport == "sse" || server.transport == "http" then
      {
        inherit (server) url headers;
        type = server.transport;
      }
    else
      throw "Unknown MCP server transport type: ${server.transport} for server ${name}";

  convertLspServer = name: server: {
    inherit (server)
      command
      args
      filetypes
      ;
    root_markers = server.rootMarkers;
    init_options = server.initOptions;
    options = server.settings;
  };
in
{
  imports = [
    inputs.self.homeModules.ai-shared
    inputs.self.homeModules.editor-shared
  ];

  options.modules.ai.crush = {
    enable = mkEnableOption "Crush";
  };

  config = mkIf cfg.enable {
    home.packages = [ perSystem.llm-agents-nix.crush ];

    xdg.configFile."crush/crush.json".text = builtins.toJSON {
      "$schema" = "https://charm.land/crush.json";
      mcp = mapAttrs convertMcpServer aiCfg.mcpServers;
      lsp = mapAttrs convertLspServer config.modules.editors.lspServers;
    };
    xdg.configFile."crush/skills".source = aiCfg._combinedSkillsPath;
  };
}
