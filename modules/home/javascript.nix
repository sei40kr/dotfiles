{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.javascript;
in
{
  options.modules.dev.lang.javascript = {
    enable = mkEnableOption "JavaScript/TypeScript development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      biome
      nodePackages.eslint
      nodePackages.eslint_d
      nodePackages.typescript
      nodePackages.webpack-cli
    ];

    modules.editors.lspServers.ts_ls = rec {
      package = pkgs.nodePackages.typescript-language-server;
      command = "${package}/bin/typescript-language-server";
      args = [ "--stdio" ];
      filetypes = [
        "javascript"
        "javascriptreact"
        "javascript.jsx"
        "typescript"
        "typescriptreact"
        "typescript.tsx"
      ];
      rootMarkers = [
        "package.json"
        "tsconfig.json"
        "jsconfig.json"
        ".git"
      ];
    };
  };
}
