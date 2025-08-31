{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.lang.javascript;
in
{
  options.modules.dev.lang.javascript = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # TODO gatsby-cli
    # TODO prettier-eslint-cli
    user.packages = with pkgs; [
      nodePackages.create-react-app
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
