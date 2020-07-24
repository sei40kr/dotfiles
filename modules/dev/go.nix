{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.go.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.go.enable {
    my.packages = with pkgs; [ go ];

    modules.shell.zsh.zinitPluginsInit = ''
      zinit snippet OMZP::golang/golang.plugin.zsh
    '';
  };
}
