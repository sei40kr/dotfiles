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
    flatten
    mapAttrsToList
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

  tomlFormat = pkgs.formats.toml { };

  policyFile =
    let
      allowCommandRules = map (cmd: {
        toolName = "run_shell_command";
        commandPrefix = "${cmd} ";
        decision = "allow";
      }) aiCfg.permissions.allowedCommandPrefixes;

      denyCommandRules = map (cmd: {
        toolName = "run_shell_command";
        commandPrefix = "${cmd} ";
        decision = "deny";
      }) aiCfg.permissions.deniedCommandPrefixes;

      mcpAllowRules = flatten (
        mapAttrsToList (
          name: server:
          map (tool: {
            toolName = tool;
            mcpName = name;
            decision = "allow";
          }) server.allowedTools
        ) aiCfg.mcpServers
      );

      mcpDenyRules = flatten (
        mapAttrsToList (
          name: server:
          map (tool: {
            toolName = tool;
            mcpName = name;
            decision = "deny";
          }) server.deniedTools
        ) aiCfg.mcpServers
      );
    in
    tomlFormat.generate "nix-managed.toml" {
      rule = allowCommandRules ++ denyCommandRules ++ mcpAllowRules ++ mcpDenyRules;
    };

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
        security.auth.selectedType = "oauth-personal";
      };
    };
    home.file.".gemini/skills".source = aiCfg._combinedSkillsPath;
    home.file.".gemini/policies/nix-managed.toml".source = policyFile;
  };
}
