{ lib, stdenv, ... }:

with lib; {
  fzf-cd-dirs = stdenv.mkDerivation {
    pname = "fzf-cd-dirs";
    version = "unstable-2020-11-22";

    src = ./zsh-fzf-cd-dirs;

    dontBuild = true;

    installPhase = ''
      mkdir -p "''${out}/share/zsh/plugins/fzf-cd-dirs"
      cp -rt "''${out}/share/zsh/plugins/fzf-cd-dirs" \
        fzf-cd-dirs.plugin.zsh functions
    '';

    meta.platforms = platforms.all;
  };

  fzf-docker = stdenv.mkDerivation {
    pname = "fzf-docker";
    version = "unstable-2020-11-22";

    src = ./zsh-fzf-docker;

    dontBuild = true;

    installPhase = ''
      mkdir -p "''${out}/share/zsh/plugins/fzf-docker"
      cp -rt "''${out}/share/zsh/plugins/fzf-docker" \
        fzf-docker.plugin.zsh functions
    '';

    meta.platforms = platforms.all;
  };

  fzf-projects = stdenv.mkDerivation {
    pname = "fzf-projects";
    version = "unstable-2020-11-22";

    src = ./zsh-fzf-projects;

    dontBuild = true;

    installPhase = ''
      mkdir -p "''${out}/share/zsh/plugins/fzf-projects"
      cp -rt "''${out}/share/zsh/plugins/fzf-projects" \
        fzf-projects.plugin.zsh functions
    '';

    meta.platforms = platforms.all;
  };

  gh-clone = stdenv.mkDerivation {
    pname = "gh-clone";
    version = "unstable-2020-11-22";

    src = ./zsh-gh-clone;

    dontBuild = true;

    installPhase = ''
      mkdir -p "''${out}/share/zsh/plugins/gh-clone"
      cp -rt "''${out}/share/zsh/plugins/gh-clone" gh-clone.plugin.zsh functions
    '';

    meta.platforms = platforms.all;
  };

  lazy-nvm = stdenv.mkDerivation {
    pname = "lazy-nvm";
    version = "unstable-2020-11-22";

    src = ./zsh-lazy-nvm;

    dontBuild = true;

    installPhase = ''
      install -D lazy-nvm.plugin.zsh \
        -T "''${out}/share/zsh/plugins/lazy-nvm/lazy-nvm.plugin.zsh"
    '';

    meta.platforms = platforms.all;
  };

  ranger-cd = stdenv.mkDerivation {
    pname = "ranger-cd";
    version = "unstable-2020-11-22";

    src = ./zsh-ranger-cd;

    dontBuild = true;

    installPhase = ''
      mkdir -p "''${out}/share/zsh/plugins/ranger-cd"
      cp -rt "''${out}/share/zsh/plugins/ranger-cd" \
        ranger-cd.plugin.zsh functions
    '';

    meta.platforms = platforms.all;
  };

  smart-command-history = stdenv.mkDerivation {
    pname = "smart-command-history";
    version = "unstable-2020-11-22";

    src = ./zsh-smart-command-history;

    dontBuild = true;

    installPhase = ''
      install -D smart-command-history.plugin.zsh \
        -T "''${out}/share/zsh/plugins/smart-command-history/smart-command-history.plugin.zsh"
    '';

    meta.platforms = platforms.all;
  };
}
