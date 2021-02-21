{ fd, fetchFromGitHub, fzf, lib, makeWrapper, tmuxPlugins, ... }:

with lib; {
  cleanup-unnamed-sessions = tmuxPlugins.mkDerivation {
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

  per-project-session = tmuxPlugins.mkDerivation {
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
    dependencies = [ fd fzf ];

    postInstall = ''
      wrapProgram "''${target}/libexec/switch-session" \
        --prefix PATH : ${makeBinPath [ fd fzf ]}
    '';
  };

  doom-one-dark = tmuxPlugins.mkDerivation {
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
