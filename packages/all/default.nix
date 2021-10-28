{ pkgs, ... }:

{
  python3Packages = (pkgs.callPackage (import ./python3-packages.nix) { });
  tmuxPlugins = (pkgs.callPackage (import ./tmux-plugins) { });
  vimPlugins = (pkgs.callPackage (import ./vim-plugins.nix) { });

  emacs = pkgs.callPackage ./emacs.nix { };
  groovy-language-server = pkgs.callPackage ./groovy-language-server { };
  idea-doom-emacs = pkgs.callPackage ./idea-doom-emacs.nix { };
  kotlin-language-server = pkgs.callPackage ./kotlin-language-server.nix { };
  zsh-fzf-chdir = pkgs.callPackage ./zsh-fzf-chdir.nix { };
  zsh-fzf-docker = pkgs.callPackage ./zsh-fzf-docker.nix { };
  zsh-fzf-projects = pkgs.callPackage ./zsh-fzf-projects.nix { };
  zsh-gh-clone = pkgs.callPackage ./zsh-gh-clone.nix { };
  zsh-ranger-cd = pkgs.callPackage ./zsh-ranger-cd.nix { };
  zsh-smart-history = pkgs.callPackage ./zsh-smart-history.nix { };
  zsh-tmux-man = pkgs.callPackage ./zsh-tmux-man.nix { };
}
