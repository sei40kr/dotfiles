{
  config,
  inputs,
  lib,
  perSystem,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mapAttrs
    ;
  aiCfg = config.modules.ai;
  cfg = aiCfg.codex;

  # Convert MCP server config to Codex format
  convertMcpServer =
    name:
    { transport, ... }@server:
    if transport == "stdio" then
      {
        inherit (server) command args env;
      }
    else if transport == "http" || transport == "sse" then
      {
        inherit (server) url;
        http_headers = server.headers;
      }
    else
      throw "Unknown transport type ${transport} for server ${name}";
in
{
  imports = [
    inputs.self.homeModules.ai-shared
  ];

  options.modules.ai.codex = {
    enable = mkEnableOption "Codex";
  };

  config = mkIf cfg.enable {
    programs.codex = {
      enable = true;
      package = perSystem.llm-agents-nix.codex;
      settings = {
        mcp_servers = mapAttrs convertMcpServer aiCfg.mcpServers;
        tui.notifications = true;
      };
    };
  };
}
