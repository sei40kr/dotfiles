{
  config,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.kotlin;
in
{
  options.modules.dev.lang.kotlin = {
    enable = mkEnableOption "Kotlin development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      kotlin
      gradle
      ktlint
      maven
    ];

    modules.editors.lspServers.kotlin_lsp = rec {
      package = perSystem.self.kotlin-lsp;
      command = "${package}/bin/intellij-server";
      args = [ "--stdio" ];
      filetypes = [ "kotlin" ];
      rootMarkers = [
        "settings.gradle"
        "settings.gradle.kts"
        "build.xml"
        "pom.xml"
        "build.gradle"
        "build.gradle.kts"
      ];
    };
  };
}
