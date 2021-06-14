{ fetchFromGitHub, lib, stdenv, zsh, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zinit";
  version = "v3.7";

  src = fetchFromGitHub {
    owner = "zdharma";
    repo = "zinit";
    rev = "1641f10c7a77ba3edcacba4f4347eef2bb620c74";
    sha256 = "04bbnk118rb9yj4y14s2x7xfnc0283a5vcayv9vc9w4l7wdi7rq7";
  };

  nativeBuildInputs = [ zsh ];

  buildPhase = ''
    ${zsh}/bin/zsh -c 'zcompile zinit.zsh'
    ${zsh}/bin/zsh -c 'zcompile zinit-side.zsh'
    ${zsh}/bin/zsh -c 'zcompile zinit-install.zsh'
    ${zsh}/bin/zsh -c 'zcompile zinit-autoload.zsh'
  '';

  installPhase = ''
    mkdir -p $out/share/zinit
    cp -r . $out/share/zinit
  '';

  meta = {
    description =
      "Ultra-flexible and fast Zsh plugin manager with clean fpath, reports, completion management, Turbo, annexes, services, packages.";
    homepage = "https://zdharma.org/zinit/wiki/";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
