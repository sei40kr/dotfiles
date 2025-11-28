{
  config,
  lib,
  inputs',
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

  # Convert Nix MCP server format to Crush format
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

  # Convert Nix LSP server format to Crush format
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
  options.modules.ai.crush = {
    enable = mkEnableOption "Crush";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ inputs'.llm-agents-nix.packages.crush ];

    home.configFile."crush/crush.json".text = builtins.toJSON {
      "$schema" = "https://charm.land/crush.json";
      mcp = mapAttrs convertMcpServer aiCfg.mcpServers;
      lsp = mapAttrs convertLspServer config.modules.editors.lspServers;
    };
  };
}
