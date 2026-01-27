{
  config,
  inputs,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mapAttrs
    ;
  aiCfg = config.modules.ai;
  cfg = aiCfg.gemini-cli;
  editorsCfg = config.modules.editors;

  notificationScript = pkgs.writeShellScript "gemini-cli-notification" ''
    message=$(${pkgs.jq}/bin/jq -r '.message' 2>/dev/null || echo "Action required or input idle.")
    ${pkgs.libnotify}/bin/notify-send 'Gemini CLI' "$message" -h string:x-dunst-stack-tag:gemini_cli
  '';

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
          preferredEditor = editorsCfg.defaultEditor;
          enableAutoUpdate = false;
          checkpointing.enabled = true;
        };
        ui = {
          showLineNumbers = true;
          footer.hideContextPercentage = false;
        };
        tools.autoAccept = true;
        experimental.skills = true;
        hooks = {
          AfterAgent = [
            {
              hooks = [
                {
                  name = "completion-notification";
                  type = "command";
                  command = "${pkgs.libnotify}/bin/notify-send 'Gemini CLI' 'Response complete!' -h string:x-dunst-stack-tag:gemini_cli";
                  description = "Send desktop notification when agent response completes";
                }
              ];
            }
          ];
          Notification = [
            {
              hooks = [
                {
                  name = "action-required-notification";
                  type = "command";
                  command = "${notificationScript}";
                  description = "Send desktop notification with message from Gemini CLI";
                }
              ];
            }
          ];
        };
        mcpServers = mapAttrs convertMcpServer aiCfg.mcpServers;
      };
    };
    home.file.".gemini/skills".source = aiCfg._combinedSkillsPath;
  };
}
