{ fetchFromGitHub, gnome, glib, lib, nodejs, nodePackages, pkgs, stdenv }:
with lib;
let
  version = "40.a";
  src = fetchFromGitHub {
    owner = "material-shell";
    repo = "material-shell";
    rev = version;
    sha256 = "Plzjid2X6C5NM/OWaMk78FKUwNT1taubFBtYw1dNIRw=";
  };
  nodeDependencies = (import ./node-composition.nix {
    inherit pkgs;
    inherit (stdenv.hostPlatform) system;
  }).nodeDependencies.override (_oldAttrs: { inherit src; });
in stdenv.mkDerivation rec {
  inherit src version;
  pname = "gnome-shell-extension-material-shell";

  patches = [ ./logging.patch ];

  buildInputs = [ glib.dev nodejs nodeDependencies ];
  buildPhase = ''
    runHook preBuild
    ln -s ${nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${nodeDependencies}/bin:$PATH"
    make compile
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp -r dist/* $out/share/gnome-shell/extensions/${uuid}
    runHook postInstall
  '';

  uuid = "material-shell@papyelgringo";

  meta = {
    description = "A modern desktop interface for Linux";
    license = licenses.gpl3;
    homepage = "https://material-shell.com";
    platforms = gnome.gnome-shell.meta.platforms;
  };
}
