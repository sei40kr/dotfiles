{ fetchFromGitHub, lib, stdenv, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zsh-smart-history";
  version = "unstable-2021-06-08";

  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "zsh-smart-history";
    rev = "c8625305797b1d8f822c7575e85b26e5b32560c7";
    sha256 = "1flngsglmhawf5jma901s9i0q9sn3c1iylnzcb91426wlrzz56g2";
  };

  dontBuild = true;

  installPhase = ''
    install -D smart-history.plugin.zsh \
      -T "''${out}/share/zsh/plugins/zsh-smart-history/smart-history.plugin.zsh"
  '';

  meta.platforms = platforms.all;
}
