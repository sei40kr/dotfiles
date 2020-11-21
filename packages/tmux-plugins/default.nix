{ fd, fzf, lib, makeWrapper, tmuxPlugins, ... }:

with lib; {
  per-project-session = tmuxPlugins.mkDerivation {
    pluginName = "per-project-session";
    rtpFilePath = "per-project-session.tmux";
    version = "unstable-2020-05-26";

    src = ./tmux-per-project-session;

    nativeBuildInputs = [ makeWrapper ];
    dependencies = [ fd fzf ];

    postInstall = ''
      wrapProgram "''${target}/libexec/goto-session.bash" \
        --prefix PATH : ${makeBinPath [ fd fzf ]}
    '';
  };
}
