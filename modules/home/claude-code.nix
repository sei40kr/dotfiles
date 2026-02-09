{
  config,
  perSystem,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    flatten
    mapAttrs
    mapAttrsToList
    mkEnableOption
    mkIf
    mkPackageOption
    ;
  cfg = config.modules.ai.claude-code;
  aiCfg = config.modules.ai;
  mcpCfg = aiCfg.mcpServers;

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
      enable = mkEnableOption "ccstatusline";
      package = mkPackageOption perSystem.llm-agents-nix "ccstatusline" { };
    };
  };

  config = mkIf cfg.enable {
    programs.claude-code = {
      enable = true;
      package = perSystem.llm-agents-nix.claude-code;
      settings = {
        env = {
          CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL = 1;
          DISABLE_AUTOUPDATER = 1;
        };
        permissions = {
          allow =
            (map (cmd: "Bash(${cmd} *)") aiCfg.permissions.allowedCommandPrefixes)
            ++ (map (domain: "WebFetch(domain:${domain})") aiCfg.permissions.allowedFetchDomains)
            ++ (flatten (
              mapAttrsToList (name: server: map (tool: "mcp__${name}__${tool}") server.allowedTools) mcpCfg
            ));
          deny =
            (map (cmd: "Bash(${cmd} *)") aiCfg.permissions.deniedCommandPrefixes)
            ++ (flatten (
              mapAttrsToList (name: server: map (tool: "mcp__${name}__${tool}") server.deniedTools) mcpCfg
            ));
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
      };
      skillsDir = config.modules.ai._combinedSkillsPath;
      mcpServers = mapAttrs convertMcpServer mcpCfg;
    };

    home.packages = [
      (mkIf cfg.ccstatusline.enable cfg.ccstatusline.package)
    ];

    xdg.configFile."ccstatusline/settings.json".text = mkIf cfg.ccstatusline.enable (
      builtins.toJSON {
        version = 3;
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
