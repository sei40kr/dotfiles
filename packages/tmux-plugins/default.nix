{ fetchFromGitHub, lib, pkgs, ... }:

with lib; {
  per-project-session = pkgs.tmuxPlugins.mkDerivation {
    pluginName = "per-project-session";
    rtpFilePath = "per-project-session.tmux";
    version = "unstable-2020-05-26";
    src = ./tmux-per-project-session;
    dependencies = with pkgs; [ fd fzf ];
    postInstall = ''
      sed -i \
          -e 's|command_exists fd|true|;
              s|fd |${pkgs.fd}/bin/fd |g;
              s|fzf-tmux |${pkgs.fzf}/bin/fzf-tmux |g' \
          $target/libexec/goto-session.bash
    '';
  };
}
