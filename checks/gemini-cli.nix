{
  inputs,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (perSystem.llm-agents-nix) gemini-cli;

  hmConfig = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      {
        _module.args = {
          inherit perSystem inputs;
        };
      }
      inputs.self.homeModules.ai-shared
      inputs.self.homeModules.gemini-cli
      {
        home = {
          username = "test";
          homeDirectory = "/home/test";
          stateVersion = "24.11";
        };
        modules.ai.gemini-cli.enable = true;
        # Override skillPaths to avoid depending on anthropics-skills
        modules.ai.skillPaths = [ ];
        # Test MCP server configurations
        modules.ai.mcpServers = {
          # stdio (local) MCP server
          filesystem = {
            transport = "stdio";
            command = "npx";
            args = [
              "-y"
              "@anthropic/mcp-server-filesystem"
              "/home/test"
            ];
          };
          # SSE (remote) MCP server
          remote-sse = {
            transport = "sse";
            url = "http://localhost:8080/sse";
            headers = {
              Authorization = "Bearer test-token";
            };
          };
          # HTTP (remote) MCP server
          remote-http = {
            transport = "http";
            url = "http://localhost:3000/mcp";
          };
        };
      }
    ];
  };
in
pkgs.runCommand "gemini-cli-check"
  {
    nativeBuildInputs = [ pkgs.check-jsonschema ];
  }
  ''
    check-jsonschema --schemafile ${gemini-cli}/share/gemini-cli/settings.schema.json \
                     ${hmConfig.activationPackage}/home-files/.gemini/settings.json
    touch $out
  ''
