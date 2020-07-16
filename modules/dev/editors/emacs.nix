{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./fonts.nix ./tabnine.nix ];

  options.modules.dev.editors.emacs.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.emacs.enable {
    modules.shell.tools.ripgrep.enable = true;

    my.home.programs.emacs = {
      enable = true;
      package = pkgs.emacsUnstable.override { withXwidgets = true; };
    };
    my.packages = with pkgs; [
      fd
      # term/vterm
      libvterm
      cmake
      # tools/ansible
      python37Packages.ansible-lint
      # tools/docker
      nodePackages.dockerfile-language-server-nodejs
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
      # lang/javascript
      nodePackages.create-react-app
      # TODO Install eslint-cli
      nodePackages.eslint_d
      # TODO Install gatsby-cli
      nodePackages.gulp-cli
      nodePackages.javascript-typescript-langserver
      nodePackages.prettier
      # TODO Install prettier-eslint-cli
      nodePackages.typescript
      nodePackages.typescript-language-server
      # TODO Install tslint
      nodePackages.webpack-cli
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
      nodePackages.bash-language-server
      shellcheck
      shfmt
      # lang/sql
      python37Packages.sqlparse
      sqlint
      # lang/web
      nodePackages.vscode-html-languageserver-bin
      nodePackages.vscode-css-languageserver-bin
      nodePackages.vue-language-server
      # TODO Install stylelint-cli
      # lang/yaml
      # TODO Install yaml-language-server
    ];
  };
}
