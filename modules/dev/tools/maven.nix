{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.maven.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.maven.enable {
    modules = {
      dev.tools.jenv.plugins = [ "maven" ];

      shell.zsh.zinitPluginsInit = ''
        zinit snippet OMZP::mvn/mvn.plugin.zsh
      '';
    };

    my.packages = with pkgs; [ maven ];
  };
}
