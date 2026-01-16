{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.kotlin;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
  ];

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

    modules.editors.lspServers.kotlin_language_server = rec {
      package = pkgs.kotlin-language-server;
      command = "${package}/bin/kotlin-language-server";
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
