{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.haskell.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.haskell.enable {
    modules = {
      dev.editors.tools.packages = with pkgs;
        with pkgs.my; [
          haskellPackages.brittany
          haskellPackages.ghcide
          haskellPackages.haskell-language-server
          haskellPackages.hlint
        ];
      shell.zsh.zinitPluginsInit = ''
        zinit ice wait'''
        zinit snippet OMZP::stack/stack.plugin.zsh
        zinit ice wait'''
        zinit snippet OMZP::cabal/cabal.plugin.zsh
      '';
    };

    user.packages = with pkgs; [ ghc stack ];

    home.file.".stack/config.yaml".text = ''
      templates:
        params:
          author-email: sei40kr@gmail.com
          author-name: ${config.user.name}
          copyright: 'Copyright (c) 2021 sei40kr'
          github-username: sei40kr
    '';
  };
}
