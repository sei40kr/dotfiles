{ lib, fetchFromGitHub, stdenvNoCC }:

stdenvNoCC.mkDerivation rec {
  pname = "WhiteSur-Light-ulauncher";
  version = "unstable-2021-03-02";
  src = fetchFromGitHub {
    owner = "Raayib";
    repo = pname;
    rev = "88e6bb3ce85b73b2b98f68cfe4da7a6576f9ecc8";
    sha256 = "sha256-sjliEXujBVtlQlucvChffAcHSnCQg+GPZCQQatr0h5g=";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out
    cp theme*.css manifest.json $out
  '';

  meta = with lib; {
    homepage = "https://github.com/Raayib/WhiteSur-Light-ulauncher";
    description = "A theme for Ulauncher. WhiteSur Light theme.";
    license = licenses.mit;
  };
}
