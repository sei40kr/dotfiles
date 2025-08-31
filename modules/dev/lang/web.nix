{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.web;
in
{
  options.modules.dev.lang.web = {
    enable = mkEnableOption "Web development";

    bun.enable = mkEnableOption "Bun";

    deno.enable = mkEnableOption "Deno";
  };

  config = mkIf cfg.enable {
    # TODO stylelint-cli
    environment.systemPackages = with pkgs; [
      nodejs_20
      emmet-language-server
      nodePackages.prettier
      nodePackages.vscode-langservers-extracted
      (mkIf cfg.bun.enable bun)
      (mkIf cfg.deno.enable deno)
    ];

    modules.editors.lspServers = {
      vue_ls = rec {
        package = pkgs.vue-language-server;
        command = "${package}/bin/vue-language-server";
        args = [ "--stdio" ];
        filetypes = [ "vue" ];
        rootMarkers = [ "package.json" ];
      };

      vtsls = rec {
        package = pkgs.vtsls;
        command = "${package}/bin/vtsls";
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
          "tsconfig.json"
          "package.json"
          "jsconfig.json"
          ".git"
        ];
      };
    };
  };
}
