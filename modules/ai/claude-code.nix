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
    mkPackageOption
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

    ccstatusline = {
      enable = mkEnableOption "ccstatusline for Claude Code";
      package = mkPackageOption inputs'.llm-agents-nix.packages "ccstatusline" { };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      inputs'.llm-agents-nix.packages.claude-code
      (mkIf cfg.ccstatusline.enable cfg.ccstatusline.package)
    ];

    environment.etc."claude-code/managed-settings.json".text = builtins.toJSON (
      {
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
                  command = "${pkgs.libnotify}/bin/notify-send 'Claude Code' 'Action required or input idle.' -h string:x-dunst-stack-tag:claude_code";
                }
              ];
            }
          ];
          Stop = [
            {
              hooks = [
                {
                  type = "command";
                  command = "${pkgs.libnotify}/bin/notify-send 'Claude Code' 'Response complete!' -h string:x-dunst-stack-tag:claude_code";
                }
              ];
            }
          ];
        };
      }
      // lib.optionalAttrs cfg.ccstatusline.enable {
        statusLine = {
          type = "command";
          command = "${cfg.ccstatusline.package}/bin/ccstatusline";
          padding = 0;
        };
      }
    );
    environment.etc."claude-code/managed-mcp.json".text = builtins.toJSON {
      mcpServers = mapAttrs convertMcpServer mcpCfg;
    };

    home.configFile."ccstatusline/settings.json".text = mkIf cfg.ccstatusline.enable (
      builtins.toJSON {
        version = 3;
        # Model: Claude | Ctx: 18.6k | Ctx(u): 11.6% | Cost: $2.45
        lines = [
          [
            {
              id = "1";
              type = "model";
              color = "";
            }
            {
              id = "2";
              type = "separator";
            }
            {
              id = "3";
              type = "context-length";
              color = "";
            }
            {
              id = "6";
              type = "separator";
            }
            {
              id = "162c2f21-371b-48b0-a623-1ddba6c760b3";
              type = "context-percentage-usable";
              color = "";
            }
            {
              id = "7d86f33e-a857-484e-8dcc-f7daf6f08397";
              type = "separator";
            }
            {
              id = "b10d0d0e-ce27-427e-a66c-43d1888d11e5";
              type = "session-cost";
              color = "";
            }
          ]
          [ ]
          [ ]
        ];
        flexMode = "full-minus-40";
        compactThreshold = 60;
        colorLevel = 2;
        inheritSeparatorColors = false;
        globalBold = false;
        powerline = {
          enabled = false;
          separators = [ "" ];
          separatorInvertBackground = [ false ];
          startCaps = [ ];
          endCaps = [ ];
          autoAlign = false;
        };
      }
    );
  };
}
