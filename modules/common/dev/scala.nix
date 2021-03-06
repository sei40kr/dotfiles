{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.scala;
in {
  options.modules.dev.scala = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO scalastyle
    user.packages = with pkgs; [ scala sbt gradle maven metals scalafmt ];
    modules.shell.zsh.extraZinitCommands = ''
      zinit ice as'completion' wait'''
      zinit snippet OMZP::scala/_scala
      zinit ice svn
      zinit snippet OMZP::sbt
    '';
  };
}
