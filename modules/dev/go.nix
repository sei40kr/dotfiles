{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.go.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.go.enable {
    modules = {
      # TODO Install goimports
      dev.editors.tools.packages = with pkgs; [ gopls gore ];
      shell.zsh.zinitPluginsInit = ''
        zinit snippet OMZP::golang/golang.plugin.zsh
      '';
    };

    user.packages = with pkgs; [ go ];
  };
}
