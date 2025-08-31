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

    modules.editors.lspServers.hls = rec {
      package = pkgs.haskellPackages.haskell-language-server;
      command = "${package}/bin/haskell-language-server-wrapper";
      args = [ "--lsp" ];
      filetypes = [
        "haskell"
        "lhaskell"
      ];
      rootMarkers = [
        "hie.yaml"
        "stack.yaml"
        "cabal.project"
        "*.cabal"
        "package.yaml"
      ];
    };
  };
}
