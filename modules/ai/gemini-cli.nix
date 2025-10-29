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
      { inherit (server) command args env; }
    else if server.transport == "sse" then
      { inherit (server) url headers; }
    else if server.transport == "http" then
      {
        inherit (server) headers;
        httpUrl = server.url;
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
