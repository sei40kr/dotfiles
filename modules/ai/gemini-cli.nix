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
    ;
  cfg = config.modules.ai.gemini-cli;
  aiCfg = config.modules.ai;

  mcpServersConfig = mapAttrs (
    name: server:
    if server.transport == "stdio" then
      {
        command = server.command;
        args = server.args;
        env = server.env;
      }
    else if server.transport == "sse" then
      {
        url = server.url;
        headers = server.headers;
      }
    else if server.transport == "http" then
      {
        httpUrl = server.url;
        headers = server.headers;
      }
    else
      { }
  ) aiCfg.mcpServers;
in
{
  options.modules.ai.gemini-cli = {
    enable = mkEnableOption "Gemini CLI - Google's Gemini AI in your terminal";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ unstable.gemini-cli ];

    environment.etc."gemini-cli/settings.json".text = builtins.toJSON {
      general = {
        vimMode = true;
        disableAutoUpdate = true;
        checkpointing.enable = true;
      };
      ui.showLineNumbers = true;
      mcpServers = mcpServersConfig;
    };
  };
}
