{ fetchFromGitHub, lib, pkgs, ... }:

with lib; {
  per-project-session = pkgs.tmuxPlugins.mkDerivation {
    pluginName = "per-project-session";
    rtpFilePath = "per-project-session.tmux";
    version = "unstable-2020-05-26";
    src = fetchFromGitHub {
      owner = "sei40kr";
      repo = "tmux-per-project-session";
      rev = "dabb8845f7df8bfd58047b3588e632d729354942";
      sha256 = "1v34mjjm6a2h8dxn5a50c9xk1zzs0pjdaxgz8g3s1z8fw57xkabl";
    };
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
