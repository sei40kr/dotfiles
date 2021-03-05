{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.java;
in {
  options.modules.dev.java = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ jdk11 gradle maven ];
    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice trigger-load'!gradle'
      zinit snippet OMZP::gradle/gradle.plugin.zsh
      zinit snippet OMZP::mvn/mvn.plugin.zsh
    '';
  };
}
