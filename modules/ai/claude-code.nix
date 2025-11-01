{
  config,
  inputs',
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mapAttrs
    ;
  cfg = config.modules.ai.claude-code;
  mcpCfg = config.modules.ai.mcpServers;

  # Convert MCP server config to Claude Code format
  convertMcpServer =
    name:
    { transport, ... }@server:
    if transport == "stdio" then
      {
        inherit (server) command args env;
        type = "stdio";
      }
    else if transport == "sse" || transport == "http" then
      {
        inherit (server) url headers;
        type = transport;
      }
    else
      throw "Unknown transport type ${transport} for MCP server ${name}";
in
{
  options.modules.ai.claude-code = {
    enable = mkEnableOption "Claude Code";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ inputs'.llm-agents-nix.packages.claude-code ];

    home.file.".claude/settings.json".text = builtins.toJSON {
      env = {
        CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL = 1;
        DISABLE_AUTOUPDATER = 1;
      };
      hooks = {
        Notification = [
          {
            hooks = [
              {
                type = "command";
                command = "${pkgs.libnotify}/bin/notify-send 'Claude Code' 'Action required or input idle.'";
              }
            ];
          }
        ];
        Stop = [
          {
            hooks = [
              {
                type = "command";
                command = "${pkgs.libnotify}/bin/notify-send 'Claude Code' 'Response complete!'";
              }
            ];
          }
        ];
      };
    };

    environment.etc."claude-code/managed-mcp.json".text = builtins.toJSON {
      mcpServers = mapAttrs convertMcpServer mcpCfg;
    };
  };
}
