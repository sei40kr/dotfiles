{ buildGoModule, fetchFromGitHub, gtk3, gtk-layer-shell, lib, makeWrapper
, pkg-config, stdenv, ... }:

with lib;
buildGoModule rec {
  pname = "nwg-dock";
  version = "v0.1.5";

  src = fetchFromGitHub {
    owner = "nwg-piotr";
    repo = "nwg-dock";
    rev = version;
    sha256 = "089c418jz0cs3gw0q17zm2b96bjfb23w9c5ni4zgim8lrxx0lbnk";
  };

  vendorSha256 = "sha256-HyrjquJ91ddkyS8JijHd9HjtfwSQykXCufa2wzl8RNk=";

  nativeBuildInputs = [ makeWrapper pkg-config ];
  propagatedBuildInputs = [ gtk-layer-shell gtk3 ];

  postInstall = ''
    mkdir -p $out/share/nwg-dock
    cp -r images $out/share/nwg-dock
    cp config/* $out/share/nwg-dock

    wrapProgram $out/bin/nwg-dock --set XDG_DATA_HOME $out/share
  '';

  meta = {
    description = "GTK3-based dock for sway";
    homepage = "https://github.com/nwg-piotr/nwg-dock";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
