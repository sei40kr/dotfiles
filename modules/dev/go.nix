{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.go.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.go.enable {
    modules = {
      # TODO goimports
      dev.editors.tools.packages = with pkgs.unstable; [
        unstable.gopls
        unstable.gore
      ];
      shell.zsh.zinitPluginsInit = ''
        zinit snippet OMZP::golang/golang.plugin.zsh
      '';
    };

    my.packages = with pkgs; [ go ];
  };
}
