{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.gradle.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.gradle.enable {
    my.packages = with pkgs; [ gradle ];

    modules.dev.tools.jenv.pluginsToEnable = [ "gradle" ];

    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice trigger-load'!gradle'
      zinit snippet OMZP::gradle/gradle.plugin.zsh
    '';
  };
}
