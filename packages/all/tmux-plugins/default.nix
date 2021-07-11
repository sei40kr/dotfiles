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

  per-project-session = tmuxPlugins.mkTmuxPlugin {
    pluginName = "per-project-session";
    rtpFilePath = "per-project-session.tmux";
    version = "unstable-2021-01-24";
    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "tmux-per-project-session";
      rev = "4fca3b42f1510a7ba4820264284f30fe60556a2d";
      sha256 = "0yb2zpvwgh5k7nnr0y3sqkx3z77ig5dwakcc3mhqlr42y1g9d1b3";
    };
    nativeBuildInputs = [ makeWrapper ];
    postInstall = ''
      wrapProgram "''${target}/libexec/switch-session" \
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
