{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.prisma;
in
{
  options.modules.dev.lang.prisma = {
    enable = mkEnableOption "Prisma";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      prisma-engines
    ];

    environment.variables = {
      PRISMA_QUERY_ENGINE_BINARY = "${pkgs.prisma-engines}/bin/query-engine";
      PRISMA_QUERY_ENGINE_LIBRARY = "${pkgs.prisma-engines}/lib/libquery_engine.node";
      PRISMA_SCHEMA_ENGINE_BINARY = "${pkgs.prisma-engines}/bin/schema-engine";
    };

    modules.editors.lspServers.prismals = rec {
      package = pkgs.nodePackages."@prisma/language-server";
      command = "${package}/bin/prisma-language-server";
      args = [ "--stdio" ];
      filetypes = [ "prisma" ];
      rootMarkers = [
        ".git"
        "package.json"
      ];
    };
  };
}
