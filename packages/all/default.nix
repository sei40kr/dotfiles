{ pkgs, ... }:

rec {
  python3Packages =
    (pkgs.python3Packages.callPackage (import ./python3-packages.nix) { });
  tmuxPlugins = (pkgs.callPackage (import ./tmux-plugins) { });
  vimPlugins = (pkgs.callPackage (import ./vim-plugins.nix) { });

  emacs = pkgs.callPackage ./emacs.nix { };
  groovy-language-server = pkgs.callPackage ./groovy-language-server { };
  idea-doom-emacs = pkgs.callPackage ./idea-doom-emacs.nix { };
  kotlin-language-server = pkgs.callPackage ./kotlin-language-server.nix { };

  online-judge-tools =
    pkgs.python3Packages.toPythonApplication python3Packages.online-judge-tools;

  online-judge-template-generator =
    pkgs.python3Packages.callPackage ./online-judge-template-generator {
      inherit (python3Packages) online-judge-api-client online-judge-tools;
    };

  online-judge-verify-helper =
    pkgs.python3Packages.callPackage ./online-judge-verify-helper {
      inherit (python3Packages) importlab online-judge-tools;
    };

  zsh-fzf-chdir = pkgs.callPackage ./zsh-fzf-chdir.nix { };
  zsh-fzf-docker = pkgs.callPackage ./zsh-fzf-docker.nix { };
  zsh-fzf-projects = pkgs.callPackage ./zsh-fzf-projects.nix { };
  zsh-gh-clone = pkgs.callPackage ./zsh-gh-clone.nix { };
  zsh-ranger-cd = pkgs.callPackage ./zsh-ranger-cd.nix { };
  zsh-smart-history = pkgs.callPackage ./zsh-smart-history.nix { };
  zsh-tmux-man = pkgs.callPackage ./zsh-tmux-man.nix { };
}
