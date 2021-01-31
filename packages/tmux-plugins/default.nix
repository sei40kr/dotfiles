{ fd, fzf, lib, makeWrapper, tmuxPlugins, ... }:

with lib; {
  cleanup-unnamed-sessions = tmuxPlugins.mkDerivation {
    pluginName = "cleanup-unnamed-sessions";
    rtpFilePath = "cleanup-unnamed-sessions.tmux";
    version = "unstable-2021-01-23";

    src = ./tmux-cleanup-unnamed-sessions;
  };

  per-project-session = tmuxPlugins.mkDerivation {
    pluginName = "per-project-session";
    rtpFilePath = "per-project-session.tmux";
    version = "unstable-2021-01-23";

    src = ./tmux-per-project-session;

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
    version = "unstable-2020-11-22";

    src = ./tmux-doom-one-dark;
  };
}
