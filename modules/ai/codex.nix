{
  config,
  lib,
  inputs,
  inputs',
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mapAttrs
    ;
  inherit (inputs.nix-std.lib.serde) toTOML;
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
        http_headers = server.headers or { };
      }
    else
      throw "Unknown transport type ${transport} for server ${name}";

  codexConfig = {
    mcp_servers = mapAttrs convertMcpServer aiCfg.mcpServers;
  };
in
{
  options.modules.ai.codex = {
    enable = mkEnableOption "Codex CLI";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ inputs'.llm-agents-nix.packages.codex ];

    # FIXME: Codex overwrites config on each run, so we need to place it in another location
    home.file.".codex/config.toml".text = toTOML codexConfig;
  };
}
