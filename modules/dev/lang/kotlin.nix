{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.lang.kotlin;
in
{
  options.modules.dev.lang.kotlin = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
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
