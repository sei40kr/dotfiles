{ fetchFromGitHub, lib, stdenv, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zsh-ranger-cd";
  version = "unstable-2020-01-24";

  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "zsh-ranger-cd";
    rev = "55ada43e66b6509e2cfcb6942ecb596597e20a10";
    sha256 = "0h2r21j12apzhv4vs99hagvngkgkl0yff90xqpfi6w8yj810imgm";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/zsh/site-functions
    cp functions/ranger-cd $out/share/zsh/site-functions
  '';

  meta.platforms = platforms.all;
}
