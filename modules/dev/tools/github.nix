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
    mkOption
    types
    ;
  cfg = config.modules.dev.tools.github;
in
{
  options.modules.dev.tools.github = {
    enable = mkEnableOption "GitHub tools";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      gh
      actionlint
      pinact
      github-mcp-server
    ];

    age.secrets.github-pat = {
      file = ../../../config/github/github-pat.age;
      owner = config.user.name;
    };

    # Configure GitHub MCP server
    modules.ai.mcpServers = {
      github = rec {
        transport = "stdio";
        package = pkgs.writeShellScriptBin "github-mcp-server-wrapper" ''
          if [ -n "$GITHUB_PAT" ]; then
            export GITHUB_PERSONAL_ACCESS_TOKEN="$GITHUB_PAT"
          else
            export GITHUB_PERSONAL_ACCESS_TOKEN="$(cat ${config.age.secrets.github-pat.path})"
          fi
          exec "${pkgs.github-mcp-server}/bin/github-mcp-server" "$@"
        '';
        command = "${package}/bin/github-mcp-server-wrapper";
        args = [ ];
      };
    };
  };
}
