{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.maven.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.maven.enable {
    my.packages = with pkgs; [ maven ];

    modules.dev.tools.jenv.pluginsToEnable = [ "maven" ];

    modules.shell.zsh.zinitPluginsInit = ''
      zinit snippet OMZP::mvn/mvn.plugin.zsh
    '';
  };
}
