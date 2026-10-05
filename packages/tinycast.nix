{ pkgs }:

let
  version = "0.10.3-beta.72";
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "tinycast";
  inherit version;

  src = pkgs.fetchurl {
    url = "https://github.com/abue-ammar/tinycast/releases/download/v${version}/Tinycast-${version}.dmg";
    hash = "sha256-MMayl8yMZApInUH2SGzIycmvvgUWAR0E951pqwKEHQY=";
  };

  # Tinycast.dmg is APFS formatted, which is unsupported by undmg.
  nativeBuildInputs = [ pkgs._7zz ];
  sourceRoot = "Tinycast Beta.app";

  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  dontFixup = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/Applications/Tinycast Beta.app"
    cp -R . "$out/Applications/Tinycast Beta.app"

    runHook postInstall
  '';

  meta = {
    description = "Native macOS launcher, hotkeys, and clipboard history";
    homepage = "https://github.com/abue-ammar/tinycast";
    license = pkgs.lib.licenses.agpl3Only;
    platforms = [
      "aarch64-darwin"
      "x86_64-darwin"
    ];
    sourceProvenance = with pkgs.lib.sourceTypes; [ binaryNativeCode ];
  };
}
