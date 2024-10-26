{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.lang.haskell;
in
{
  options.modules.dev.lang.haskell = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ghc
      stack
      ormolu
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
