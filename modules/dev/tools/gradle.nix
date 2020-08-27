{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.gradle.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.gradle.enable {
    modules = {
      dev.tools.jenv.pluginsToEnable = [ "gradle" ];
      shell.zsh.zinitPluginsInit = ''
        zinit ice trigger-load'!gradle'
        zinit snippet OMZP::gradle/gradle.plugin.zsh
      '';
    };

    my.packages = with pkgs; [ gradle ];
  };
}
