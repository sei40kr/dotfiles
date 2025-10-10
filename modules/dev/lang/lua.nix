{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.dev.lang.lua;
in
{
  options.modules.dev.lang.lua = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      lua
      stylua
    ];

    modules.editors.lspServers.lua_ls = rec {
      package = pkgs.sumneko-lua-language-server;
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
