{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.java;
in
{
  options.modules.dev.lang.java = {
    enable = mkEnableOption "Java development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      jdk11
      gradle
      maven
    ];

    home.shellAliases.mvnag = "mvn archetype:generate";
  };
}
