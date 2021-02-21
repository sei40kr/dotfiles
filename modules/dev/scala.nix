{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.scala.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.scala.enable {
    # TODO scalastyle
    modules = {
      dev = {
        editors.tools.packages = with pkgs; [ metals scalafmt ];
        tools = {
          jenv = {
            enable = mkForce true;
            plugins = [ "scala" "sbt" ];
          };
          maven.enable = mkForce true;
          gradle.enable = mkForce true;
        };
      };
      shell.zsh.zinitPluginsInit = ''
        zinit ice as'completion' wait'''
        zinit snippet OMZP::scala/_scala
        zinit ice svn
        zinit snippet OMZP::sbt
      '';
    };

    user.packages = with pkgs; [ scala sbt scalafmt metals ];
  };
}
