{ pkgs, uPkgs }:

# TODO refactoring
let
  python3Packages = (pkgs.callPackage (import ./python3-packages.nix) { });
  tmuxPlugins = (pkgs.callPackage (import ./tmux-plugins) { });
  vimPlugins = (pkgs.callPackage (import ./vim-plugins.nix) { });
  vscode-extensions = (pkgs.callPackage (import ./vscode-extensions.nix) { });
  zshPlugins = (pkgs.callPackage (import ./zsh-plugins) { });
in {
  inherit python3Packages tmuxPlugins vimPlugins vscode-extensions zshPlugins;

  alfred = pkgs.callPackage ./alfred.nix { };
  corretto_11 = pkgs.callPackage ./corretto_11.nix { };
  emacs = pkgs.callPackage ./emacs.nix { emacs = uPkgs.emacs; };
  kotlin-language-server =
    pkgs.callPackage ./kotlin-language-server.nix { gradle_6 = uPkgs.gradle; };
  notion = pkgs.callPackage ./notion.nix { };
  operator-mono = pkgs.callPackage ./operator-mono.nix { };
  zinit = pkgs.callPackage ./zinit.nix { };
}
