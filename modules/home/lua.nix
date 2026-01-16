{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.lua;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
  ];

  options.modules.dev.lang.lua = {
    enable = mkEnableOption "Lua development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      lua
      stylua
    ];

    modules.editors.lspServers.lua_ls = rec {
      package = pkgs.lua-language-server;
      command = "${package}/bin/lua-language-server";
      filetypes = [ "lua" ];
      rootMarkers = [
        ".luarc.json"
        ".luarc.jsonc"
        ".luacheckrc"
        ".stylua.toml"
        "stylua.toml"
        "selene.toml"
        "selene.yml"
        ".git"
      ];
    };
  };
}
