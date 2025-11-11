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
    enable = mkEnableOption "Gemini CLI - Google's Gemini AI in your terminal";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ inputs'.llm-agents-nix.packages.gemini-cli ];

    environment.etc."gemini-cli/settings.json".text = builtins.toJSON {
      general = {
        vimMode = true;
        disableAutoUpdate = true;
        checkpointing.enabled = true;
      };
      ui.showLineNumbers = true;
      mcpServers = mapAttrs convertMcpServer aiCfg.mcpServers;
    };
  };
}
