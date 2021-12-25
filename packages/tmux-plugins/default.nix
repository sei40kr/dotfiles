{ fd, fetchFromGitHub, fzf, lib, makeWrapper, tmuxPlugins, ... }:

with lib; {
  cleanup-unnamed-sessions = tmuxPlugins.mkTmuxPlugin {
    pluginName = "cleanup-unnamed-sessions";
    rtpFilePath = "cleanup-unnamed-sessions.tmux";
    version = "unstable-2021-01-24";
    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "tmux-cleanup-unnamed-sessions";
      rev = "24d5a6f3ef36649dc6627760ca4fecb9b5007cf1";
      sha256 = "1vfygf2b9qfnfwsv9f4yzrqiyzm95kl571w3cw14439qr1gnpjh5";
    };
  };

  ghq = tmuxPlugins.mkTmuxPlugin {
    pluginName = "ghq";
    rtpFilePath = "ghq.tmux";
    version = "unstable-2021-12-10";

    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "tmux-ghq";
      rev = "c484d6e7b286fddbd33e37f15e5d49b29b4b7b7f";
      sha256 = "10r5hp44gvh7m1z21zkhkpi0lkcma4y66iaxnl6ivlqn3740sdqq";
    };

    nativeBuildInputs = [ makeWrapper ];

    postInstall = ''
      wrapProgram $out/share/tmux-plugins/ghq/libexec/create-or-switch-to.bash \
        --prefix PATH : ${makeBinPath [ fd fzf ]}
    '';
  };

  cowboy = tmuxPlugins.mkTmuxPlugin {
    pluginName = "cowboy";
    rtpFilePath = "cowboy.tmux";
    version = "unstable-2021-05-11";
    src = fetchFromGitHub {
      owner = "tmux-plugins";
      repo = "tmux-cowboy";
      rev = "75702b6d0a866769dd14f3896e9d19f7e0acd4f2";
      sha256 = "16wqwfaqy7nhiy1ijkng1x4baqq7s9if0m3ffcrnakza69s6r4r8";
    };
  };

  doom-one-dark = tmuxPlugins.mkTmuxPlugin {
    pluginName = "doom-one-dark";
    rtpFilePath = "doom-one-dark.tmux";
    version = "unstable-2021-01-24";
    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "tmux-doom-one-dark";
      rev = "7bbbeaf777a5baf6e833d1ab710c1b9b98084adf";
      sha256 = "1v4fmn7b34iwdbnwnkdwxxjzqf27yh46d7hivzl121378nba35n8";
    };
  };
}
