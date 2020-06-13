{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.haskell.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.haskell.enable {
    my.packages = with pkgs; [ stack ];

    my.home.home.file.".stack/config.yml".source = <config/stack/config.yaml>;

    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice wait'''
      zinit snippet OMZP::stack/stack.plugin.zsh
      zinit ice wait'''
      zinit snippet OMZP::cabal/cabal.plugin.zsh
    '';
  };
}
