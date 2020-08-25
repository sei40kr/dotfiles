{ fetchFromGitHub, lib, stdenv, zsh, ... }:

with lib;
stdenv.mkDerivation {
  pname = "zinit";
  version = "v3.1";

  src = fetchFromGitHub {
    owner = "zdharma";
    repo = "zinit";
    rev = "fbc77d998547ca546115c0fb79a17c653ab57ea1";
    sha256 = "1mpdsfg5caxli1w7dhgxiir01hdc97wqv54i3rki3fq9qrhsk2j9";
  };

  nativeBuildInputs = [ zsh ];

  buildPhase = ''
    ${zsh}/bin/zsh -c 'zcompile zinit.zsh'
    ${zsh}/bin/zsh -c 'zcompile zinit-side.zsh'
    ${zsh}/bin/zsh -c 'zcompile zinit-install.zsh'
    ${zsh}/bin/zsh -c 'zcompile zinit-autoload.zsh'
    ${zsh}/bin/zsh -c 'zcompile git-process-output.zsh'
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
