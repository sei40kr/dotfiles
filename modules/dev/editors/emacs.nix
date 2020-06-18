{ config, lib, options, pkgs, ... }:

with lib;
let
  all-hies =
    import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master")
    { };
in {
  imports = [ ./fonts.nix ];

  options.modules.dev.editors.emacs.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.emacs.enable {
    my.home.programs.emacs = {
      enable = true;
      package = pkgs.emacsUnstable;
    };

    my.packages = with pkgs; [
      ripgrep
      fd
      # term/vterm
      libvterm
      cmake
      # tools/ansible
      python37Packages.ansible-lint
      # tools/docker
      # TODO Install dockerfile-language-server
      # lang/cc
      ccls
      cpplint
      # lang/ess
      rPackages.languageserver
      rPackages.lintr
      # lang/go
      # TODO Install goimports
      # TODO Install gopls
      # TODO Install gore
      # lang/groovy
      # TODO Install groovy-language-server
      # lang/haskell
      haskellPackages.brittany
      haskellPackages.hlint
      (all-hies.selection { selector = p: p; })
      # lang/kotlin
      # TODO Install kotlin-language-server
      ktlint
      # lang/markdown
      # TODO Install markdownlint-cli
      mdl
      python37Packages.grip
      # lang/nix
      nixfmt
      # lang/plantuml
      plantuml
      # lang/python
      python37Packages.black
      python37Packages.python-language-server
      # lang/ruby
      solargraph
      rubyPackages.rubocop
      # lang/rust
      # TODO Install rust-analyzer
      # lang/scala
      metals
      scalafmt
      # TODO Install scalastyle
      # lang/sh
      # TODO Install bash-language-server
      shellcheck
      shfmt
      # lang/sql
      python37Packages.sqlparse
      sqlint
    ];
  };
}
