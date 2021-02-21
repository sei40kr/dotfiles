{ fetchFromGitHub, lib, stdenv, ... }:

with lib; {
  fzf-cd-dirs = stdenv.mkDerivation {
    pname = "fzf-cd-dirs";
    version = "unstable-2020-01-09";

    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "zsh-fzf-cd-dirs";
      rev = "c59dde5c63821d98bb2fab5dd7a3a308df8a3594";
      sha256 = "05nff854spaqcqwn0yhr6pkpqh7kd02nvh7nhr8fbi6nb2ywh98l";
    };

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
    version = "unstable-2020-01-24";

    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "zsh-fzf-docker";
      rev = "0bc3f104d7359db1306104f1fc8533a5a8498ad9";
      sha256 = "0wyma88p66z69vpi0n25nrpwrxwkrwl8fbifll5jv83q98qsg30k";
    };

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
    version = "unstable-2020-01-24";

    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "zsh-fzf-projects";
      rev = "ce061b33bd3c6c6392e4d4f3aaf90f593d4b404d";
      sha256 = "0wx9acrxj036w57h75w8xs68qij0bv9ixkcvxq1k8a3pyavhamcd";
    };

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
    version = "unstable-2020-01-25";

    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "zsh-gh-clone";
      rev = "e5fa7b5aa62f1acc388ee3dcc0f4cf4352184a53";
      sha256 = "0xw8w3ss48lsq21dhdfyxral5qqc1yjv8yz8v3vyih7lk10cnpy2";
    };

    dontBuild = true;

    installPhase = ''
      mkdir -p "''${out}/share/zsh/plugins/gh-clone"
      cp -rt "''${out}/share/zsh/plugins/gh-clone" gh-clone.plugin.zsh functions
    '';

    meta.platforms = platforms.all;
  };

  lazy-nvm = stdenv.mkDerivation {
    pname = "lazy-nvm";
    version = "unstable-2021-01-24";

    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "zsh-lazy-nvm";
      rev = "3875f8d57317971111d60400d430f0ac747a577b";
      sha256 = "1ycby996cm9jv86mibb5gjhl32jjknx1586ckkp6zk9yx91g49v4";
    };

    dontBuild = true;

    installPhase = ''
      install -D lazy-nvm.plugin.zsh \
        -T "''${out}/share/zsh/plugins/lazy-nvm/lazy-nvm.plugin.zsh"
    '';

    meta.platforms = platforms.all;
  };

  ranger-cd = stdenv.mkDerivation {
    pname = "ranger-cd";
    version = "unstable-2020-01-24";

    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "zsh-ranger-cd";
      rev = "55ada43e66b6509e2cfcb6942ecb596597e20a10";
      sha256 = "0h2r21j12apzhv4vs99hagvngkgkl0yff90xqpfi6w8yj810imgm";
    };

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
    version = "unstable-2020-12-20";

    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "zsh-smart-command-history";
      rev = "e578f2757018dad7c6c76f45f7bb5d2c8e351f6f";
      sha256 = "1sjfig9c870l6q7f9ssc8qk1d8nmrpbzwmj6abkjcsb140cgqf0v";
    };

    dontBuild = true;

    installPhase = ''
      install -D smart-command-history.plugin.zsh \
        -T "''${out}/share/zsh/plugins/smart-command-history/smart-command-history.plugin.zsh"
    '';

    meta.platforms = platforms.all;
  };

  tmux-man = stdenv.mkDerivation {
    pname = "tmux-man";
    version = "unstable-2021-01-24";

    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "zsh-tmux-man";
      rev = "41cbc1291e6e0bc6d1a1d0cb0cf48e227611968e";
      sha256 = "13nb75lv4y4aragd6arifc8avswaykk88dp52g5qcfcpywnr18nr";
    };

    dontBuild = true;

    installPhase = ''
      install -D tmux-man.plugin.zsh \
        -T "''${out}/share/zsh/plugins/tmux-man/tmux-man.plugin.zsh"
    '';

    meta.platforms = platforms.all;
  };
}
