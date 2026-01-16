{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.modules.dev.lang.haskell;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
  ];

  options.modules.dev.lang.haskell = {
    enable = mkEnableOption "Haskell development environment";
    userName = mkOption {
      type = types.str;
      default = "sei40kr";
      description = "Author name for Stack templates";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cabal-install
      haskell-language-server
      haskellPackages.ghcid
      stack
    ];

    home.file.".stack/config.yaml".text = ''
      templates:
        params:
          author-name: ${cfg.userName}
          copyright: 'Copyright (c) 2021 sei40kr'
          github-username: sei40kr
    '';

    modules.editors.lspServers.hls = rec {
      package = pkgs.haskell-language-server;
      command = "${package}/bin/haskell-language-server-wrapper";
      args = [ "--lsp" ];
      filetypes = [
        "haskell"
        "lhaskell"
      ];
      rootMarkers = [
        "*.cabal"
        "stack.yaml"
        "cabal.project"
        "package.yaml"
        "hie.yaml"
        ".git"
      ];
    };
  };
}
