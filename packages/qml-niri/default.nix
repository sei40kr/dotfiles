{
  lib,
  cmake,
  fetchFromGitHub,
  qt6,
  stdenv,
  ...
}:

stdenv.mkDerivation {
  pname = "qml-niri";
  version = "unstable-2025-10-15";

  src = fetchFromGitHub {
    owner = "imiric";
    repo = "qml-niri";
    rev = "7694e2840032210ba859a62c4b684a1a943244a0";
    hash = "sha256-O9cMtAMcGGVA0qkOvim6D+7CA0w1DvyVYUlJpNKp/A0=";
  };

  nativeBuildInputs = [
    cmake
    qt6.qtdeclarative
  ];

  dontWrapQtApps = true;

  postBuild = ''
    mkdir build
    cd build
    cmake $sourceRoot
  '';

  buildPhase = ''
    make
  '';

  installPhase = ''
    mkdir -p $out/lib/qt-6/qml
    cp -r Niri $out/lib/qt-6/qml/
  '';

  meta = with lib; {
    description = "A QML plugin for niri";
    homepage = "https://github.com/imiric/qml-niri";
    license = licenses.mit;
  };
}
