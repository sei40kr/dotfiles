{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.dev.lang.java;
in
{
  options.modules.dev.lang.java = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      jdk11
      gradle
      maven
    ];

    modules.shell.aliases.mvnag = "mvn archetype:generate";
  };
}
