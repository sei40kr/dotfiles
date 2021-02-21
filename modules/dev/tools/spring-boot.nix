{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.springBoot.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.springBoot.enable {
    modules = {
      dev.tools.jenv.plugins = [ "springboot" ];
      shell.zsh.zinitPluginsInit = ''
        zinit ice as'completion' wait'''
        zinit snippet OMZP::spring/_spring
      '';
    };

    user.packages = with pkgs; [ spring-boot ];
  };
}
