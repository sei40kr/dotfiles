{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.dev.editors.doomEmacs;
  variables = { } // cfg.variables;
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
  options.modules.dev.editors.doomEmacs = {
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
        emacs.enable = mkForce true;
        tabnine.enable = mkForce true;
        tools.enable = mkForce true;
      };
    };

    user.packages = with pkgs;
      with pkgs.my;
      [
        fd
        # tools/docker
        nodePackages.dockerfile-language-server-nodejs
        # lang/markdown
        # TODO Install markdownlint-cli
        mdl
        python37Packages.grip
        # lang/nix
        nixfmt
        # lang/plantuml
        plantuml
        # lang/sql
        python37Packages.sqlparse
        sqlint
        # lang/yaml
        # TODO Install yaml-language-server
      ] ++ optionals stdenv.isLinux [
        # term/vterm
        libvterm
        cmake
        # lang/nix
        # TODO Fix nix-linter build & install it
        #      cf. https://github.com/NixOS/nixpkgs/pull/109081
        # nix-linter
      ];
    home.file.".doom.d/nix-variables.el".text = ''
      ;;; $DOOMDIR/nix-variables.el -*- lexical-binding: t; -*-

      ${optionalString (variables != { }) ''
        (setq ${
          concatStringsSep "\n      "
          (mapAttrsToList (k: v: "${k} ${toLisp v}") variables)
        })
      ''}
    '';
  };
}
