{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.prisma;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
  ];

  options.modules.dev.lang.prisma = {
    enable = mkEnableOption "Prisma development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      prisma-language-server
      prisma-engines
    ];

    home.sessionVariables = {
      PRISMA_QUERY_ENGINE_LIBRARY = "${pkgs.prisma-engines}/lib/libquery_engine.node";
      PRISMA_QUERY_ENGINE_BINARY = "${pkgs.prisma-engines}/bin/query-engine";
      PRISMA_SCHEMA_ENGINE_BINARY = "${pkgs.prisma-engines}/bin/schema-engine";
    };

    modules.editors.lspServers.prismals = rec {
      package = pkgs.prisma-language-server;
      command = "${package}/bin/prisma-language-server";
      args = [ "--stdio" ];
      filetypes = [ "prisma" ];
      rootMarkers = [
        "package.json"
        ".git"
      ];
    };
  };
}
