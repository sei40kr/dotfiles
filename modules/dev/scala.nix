{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.scala.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.scala.enable {
    modules.dev.tools = {
      jenv = {
        enable = mkForce true;
        pluginsToEnable = [ "scala" "sbt" ];
      };
      maven.enable = mkForce true;
      gradle.enable = mkForce true;
    };

    my.packages = with pkgs; [ scala sbt scalafmt metals ];

    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice as'completion' wait'''
      zinit snippet OMZP::scala/_scala
      zinit ice svn
      zinit snippet OMZP::sbt
    '';
  };
}
