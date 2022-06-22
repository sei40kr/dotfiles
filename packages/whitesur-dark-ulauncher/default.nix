{ lib, fetchFromGitHub, stdenvNoCC }:

stdenvNoCC.mkDerivation rec {
  pname = "WhiteSur-Dark-ulauncher";
  version = "unstable-2021-03-02";
  src = fetchFromGitHub {
    owner = "Raayib";
    repo = pname;
    rev = "480ea5ae803c72497a2e6d6e75926d0eeeb2ec88";
    sha256 = "sha256-MGlef7Xjn+vStjnGvVvmDHHkzb15rNIzje4Cmf8i5E8=";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out
    cp theme*.css manifest.json $out
  '';

  meta = with lib; {
    homepage = "https://github.com/Raayib/WhiteSur-Dark-ulauncher";
    description = "A theme for Ulauncher. WhiteSur Dark theme.";
    license = licenses.mit;
  };
}
