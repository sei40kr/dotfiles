{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.astro;
in
{
  options.modules.dev.lang.astro = {
    enable = mkEnableOption "Astro";
  };

  config = mkIf cfg.enable {
    modules.editors.lspServers.astro = rec {
      package = pkgs.astro-language-server;
      command = "${package}/bin/astro-ls";
      args = [ "--stdio" ];
      filetypes = [ "astro" ];
      rootMarkers = [
        "package.json"
        "tsconfig.json"
        "jsconfig.json"
        ".git"
      ];
    };
  };
}
