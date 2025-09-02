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
    mapAttrs
    head
    tail
    ;
  aiCfg = config.modules.ai;
  cfg = aiCfg.crush;

  # Convert Nix MCP server format to Crush format
  convertMcpServer =
    name: server:
    if server.transport == "stdio" then
      {
        type = "stdio";
        command = server.command;
        args = server.args;
        env = server.env;
      }
    else if server.transport == "sse" || server.transport == "http" then
      {
        type = server.transport;
        url = server.url;
        headers = server.headers;
      }
    else
      throw "Unknown MCP server transport type: ${server.transport} for server ${name}";

  # Convert Nix LSP server format to Crush format
  convertLspServer = name: server: {
    command = server.command;
    args = server.args;
  };
in
{
  options.modules.ai.crush = {
    enable = mkEnableOption "Crush";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ unstable.crush ];

    home.configFile."crush/crush.json".text = builtins.toJSON {
      "$schema" = "https://charm.land/crush.json";
      mcp = mapAttrs convertMcpServer aiCfg.mcpServers;
      lsp = mapAttrs convertLspServer config.modules.editors.lspServers;
    };
  };
}
