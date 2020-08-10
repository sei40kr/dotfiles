{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.dev.editors.emacs;
  toLisp = v:
    if builtins.isNull v then
      "nil"
    else if builtins.isFloat v then
      toString v
    else if isInt v then
      toString v
    else if isBool v then
      (if v then "t" else "nil")
    else if isString v then
      ''"${escapeLispString v}"''
    else if isList v then
      "'(${concatMapStringsSep " " toLisp v})"
    else
      abort "toLisp: unexpected type (v = ${v})";
  escapeLispString = v:
    escape [ "\\" ''"'' ]
    (replaceStrings [ "\n" "\r" "	" ] [ "\\n" "\\r" "\\t" ] v);
in {
  options.modules.dev.editors.emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    variables = mkOption {
      type = types.attrs;
      default = { };
    };
  };

  config = mkIf cfg.enable {
    modules = {
      shell.tools.ripgrep.enable = mkForce true;
      dev.editors = {
        emacs.variables.lsp-clients-kotlin-server-executable =
          "${pkgs.my.kotlin-language-server}/bin/kotlin-language-server";
        fonts.enable = mkForce true;
        tabnine.enable = mkForce true;
      };
    };

    my.home.programs.emacs = {
      enable = true;
      package = pkgs.emacsGcc;
    };
    my.packages = with pkgs;
      with pkgs.my; [
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
        haskell-language-server
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
        ktlint
        # lang/markdown
        # TODO Install markdownlint-cli
        mdl
        python37Packages.grip
        # lang/nix
        nixfmt
        nix-linter
        # lang/jupyter
        python37Packages.jupyter
        python37Packages.numpy
        python37Packages.matplotlib
        python37Packages.pandas
        python37Packages.seaborn
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
    my.home.home.file.".doom.d/nix-env.el".text = ''
      ;;; $DOOMDIR/nix-env.el -*- lexical-binding: t; -*-

      (setq ${
        concatStringsSep "\n      "
        (mapAttrsToList (k: v: "${k} ${toLisp v}") cfg.variables)
      })
    '';
  };
}
