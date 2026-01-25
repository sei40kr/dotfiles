{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.editors.idea;
in
{
  options.modules.editors.idea = {
    enable = mkEnableOption "IntelliJ IDEA";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      (jetbrains.plugins.addPlugins jetbrains.idea [
        "acejump"
        # FIXME: NixOS/nixpkgs#400317 github-copilot fails to build
        # "github-copilot"
        "ideavim"
        "python"
        "rust"
      ])
    ];

    modules.editors.ideavim.enable = true;

    programs.git.ignores = [
      ".idea/**/workspace.xml"
      ".idea/**/tasks.xml"
      ".idea/**/usage.statistics.xml"
      ".idea/**/dictionaries"
      ".idea/**/shelf"
      ".idea/**/contentModel.xml"
      ".idea/**/dataSources/"
      ".idea/**/dataSources.ids"
      ".idea/**/dataSources.local.xml"
      ".idea/**/sqlDataSources.xml"
      ".idea/**/dynamic.xml"
      ".idea/**/uiDesigner.xml"
      ".idea/**/dbnavigator.xml"
      ".idea/**/gradle.xml"
      ".idea/**/libraries"
      ".idea/**/sonarlint/"
      ".idea/**/mongoSettings.xml"
      "*.iws"
      "out/"
      ".idea_modules/"
      "atlassian-ide-plugin.xml"
      "com_crashlytics_export_strings.xml"
      "crashlytics.properties"
      "crashlytics-build.properties"
      "fabric.properties"
      ".idea/caches/build_file_checksums.ser"
    ];
  };
}
