{ lib, buildGoModule, fetchFromGitHub, pkg-config, cairo, gobject-introspection
, gtk3, gtk-layer-shell }:

buildGoModule rec {
  pname = "nwg-drawer";
  version = "0.2.7";

  src = fetchFromGitHub {
    owner = "nwg-piotr";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-o1KrPosnGhbm5gDK8BtoDDKRafyCqO0mdlVPYzMCenY=";
  };

  patches = [ ./fix-paths.patch ];
  postPatch = ''
    substituteInPlace tools.go --subst-var out
  '';

  vendorSha256 = "sha256-Twipdrt3XZVrzJvElEGbKaJRMnop8fIFMFnriPTSS14=";

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [ cairo gobject-introspection gtk3 gtk-layer-shell ];

  doCheck = false;

  preInstall = ''
    mkdir -p $out/share/nwg-drawer
    cp -r desktop-directories drawer.css $out/share/nwg-drawer
  '';

  meta = with lib; {
    description = "Application drawer for sway Wayland compositor";
    homepage = "https://github.com/nwg-piotr/nwg-drawer";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
