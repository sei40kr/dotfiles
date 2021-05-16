{ pkgs, ... }:

{
  python3Packages = (pkgs.callPackage (import ./python3-packages.nix) { });
  tmuxPlugins = (pkgs.callPackage (import ./tmux-plugins) { });
  vimPlugins = (pkgs.callPackage (import ./vim-plugins.nix) { });
  zshPlugins = (pkgs.callPackage (import ./zsh-plugins) { });

  emacs = pkgs.callPackage ./emacs.nix { };
  groovy-language-server = pkgs.callPackage ./groovy-language-server { };
  idea-doom-emacs = pkgs.callPackage ./idea-doom-emacs.nix { };
  kotlin-language-server = pkgs.callPackage ./kotlin-language-server.nix { };
  operator-mono = pkgs.callPackage ./operator-mono.nix { };
  vscode-extensions = (pkgs.callPackage (import ./vscode-extensions.nix) { });
  zinit = pkgs.callPackage ./zinit.nix { };
}
