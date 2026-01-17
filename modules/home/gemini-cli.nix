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
  cfg = config.modules.ai.gemini-cli;
  aiCfg = config.modules.ai;

  convertMcpServer =
    name: server:
    if server.transport == "stdio" then
      { inherit (server) command args env; }
    else if server.transport == "sse" then
      { inherit (server) url headers; }
    else if server.transport == "http" then
      {
        httpUrl = server.url;
        inherit (server) headers;
      }
    else
      throw "Unknown transport type '${server.transport}' for MCP server '${name}'";
in
{
  options.modules.ai.gemini-cli = {
    enable = mkEnableOption "Gemini CLI";
  };

  config = mkIf cfg.enable {
    programs.gemini-cli = {
      enable = true;
      package = perSystem.llm-agents-nix.gemini-cli;
      settings = {
        general = {
          vimMode = true;
          disableAutoUpdate = true;
          checkpointing.enabled = true;
        };
        ui.showLineNumbers = true;
        mcpServers = mapAttrs convertMcpServer aiCfg.mcpServers;
      };
    };
  };
}
