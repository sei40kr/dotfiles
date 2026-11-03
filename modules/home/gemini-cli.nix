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
          enableNotifications = true;
          notificationMethod = "osc9";
        };
        ui = {
          showLineNumbers = true;
          footer.hideContextPercentage = false;
        };
        tools.autoAccept = true;
        experimental.skills = true;
        mcpServers = mapAttrs convertMcpServer aiCfg.mcpServers;
        security.auth.selectedType = "oauth-personal";
      };
    };
    # Claude Code gets these skill repos through its plugin marketplace
    # mechanism instead, so they are wired here rather than into the shared
    # modules.ai.skillPaths set.
    home.file.".gemini/skills".source = pkgs.symlinkJoin {
      name = "gemini-skills";
      paths = [
        aiCfg._combinedSkillsPath
        "${inputs.skills}/skills"
        (pkgs.linkFarm "ponytail-skills" [
          {
            name = "ponytail";
            path = "${inputs.ponytail}/skills/ponytail";
          }
        ])
        # yomiyasu keeps SKILL.md at its repo root alongside a test corpus and a
        # nested skills/ directory, so pick out only what SKILL.md references
        (pkgs.linkFarm "yomiyasu-skills" [
          {
            name = "yomiyasu";
            path = pkgs.linkFarm "yomiyasu" [
              {
                name = "SKILL.md";
                path = "${inputs.yomiyasu}/SKILL.md";
              }
              {
                name = "references";
                path = "${inputs.yomiyasu}/references";
              }
              {
                name = "scripts";
                path = "${inputs.yomiyasu}/scripts";
              }
            ];
          }
        ])
      ];
    };
    home.file.".gemini/policies/nix-managed.toml".source = policyFile;
  };
}
