{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.haskell;
in {
  options.modules.dev.haskell = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ghc
      stack
      haskellPackages.brittany
      haskellPackages.haskell-language-server
      haskellPackages.hlint
    ];
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
