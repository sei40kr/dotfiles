{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./fonts.nix ./tabnine.nix ];

  options.modules.dev.editors.emacs.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.emacs.enable {
    my.home.programs.emacs = {
      enable = true;
      package = pkgs.emacsUnstable.override { withXwidgets = true; };
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
      unstable.gopls
      unstable.gore
      # lang/groovy
      # TODO Install groovy-language-server
      # lang/haskell
      haskellPackages.brittany
      haskellPackages.hlint
      haskellPackages.ghcide
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
      unstable.rust-analyzer
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
