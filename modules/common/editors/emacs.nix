{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (pkgs.stdenv) isDarwin;
  editorsCfg = config.modules.editors;
  cfg = editorsCfg.emacs;
  # 28 + native-comp + pgtk
  package = if cfg.doom.enable then
    ((pkgs.emacsPackagesGen pkgs.my.emacs).emacsWithPackages (epkgs:
      with epkgs; [
        melpaPackages.emacsql-sqlite
        melpaPackages.vterm
        (melpaPackages.zmq.overrideAttrs ({ postInstall ? "", ... }: {
          postInstall = postInstall + (optionalString isDarwin ''

            (
              cd $out/share/emacs/site-lisp/elpa/zmq-*
              mv emacs-zmq.so emacs-zmq.dylib
            )
          '');
        }))
      ]))
  else
    pkgs.my.emacs;
in {
  options.modules.editors.emacs = with types; {
    enable = mkBoolOpt false;
    doom = {
      enable = mkBoolOpt false;
      variables = mkOpt attrs { };
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [ package binutils ] ++ optionals cfg.doom.enable [
        ## Doom dependencies
        fd
        ripgrep

        ## Module dependencies
        # :input japanese
        cmigemo
        # :lang json
        nodePackages.vscode-json-languageserver-bin
        # :lang markdown
        mdl
        python3Packages.grip
        # :lang nix
        nixfmt
        nix-linter
        rnix-lsp
        # :lang plantuml
        plantuml
        # :lang sql
        sqlint
        python3Packages.sqlparse
        # :lang yaml
        nodePackages.yaml-language-server
        # :checkers spell
        aspell
        # :tools docker
        nodePackages.dockerfile-language-server-nodejs
        # :tools editorconfig
        editorconfig-core-c
        # :tools lookup +docsets
        sqlite
        # :tools lookup +dictionary +offline
        wordnet
      ];
    home.file.".doom.d/nix-doom-vars.el" =
      mkIf (cfg.doom.enable && cfg.doom.variables != { }) {
        text = ''
          (setq doom-font (font-spec
                  :family ${toEmacsLisp editorsCfg.fonts.code.family}
                  :size ${toEmacsLisp editorsCfg.fonts.code.size}.0)
                doom-variable-pitch-font (font-spec
                  :family ${toEmacsLisp editorsCfg.fonts.ui.family}
                  :size ${toEmacsLisp editorsCfg.fonts.ui.size}.0)
                ${
                  (concatStringsSep "\n      "
                    (mapAttrsToList (k: v: "${k} ${toEmacsLisp v}")
                      cfg.doom.variables))
                })
        '';
      };
    fonts.fonts = with pkgs; [ emacs-all-the-icons-fonts ];
    modules.editors.fonts.enable = true;
  };
}
