{ pkgs }:

let
  version = "0.1.0";
  build = "2116";
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "rex-app";
  version = "${version}-${build}";

  # From the preview appcast, https://blob.superlogical.com/public/rex-app/preview/appcast.xml
  src = pkgs.fetchurl {
    url = "https://blob.superlogical.com/public/rex-app/preview/Rex-${version}-${build}.dmg";
    hash = "sha256-ZCSwV9+FGURpZlExAKUkZTlTAiDTxr5kfnj4oDLp/O0=";
  };

  # Rex.dmg is LZMA-compressed (ULMO), which is unsupported by undmg.
  nativeBuildInputs = [ pkgs._7zz ];
  sourceRoot = "Rex/Rex.app";

  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  # Keep the bundle byte-identical so its code signature stays valid.
  dontFixup = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/Applications/Rex.app"
    cp -R . "$out/Applications/Rex.app"

    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Rex, the multiplexer for all work";
    homepage = "https://www.superlogical.com/rex";
    license = pkgs.lib.licenses.unfree;
    platforms = [ "aarch64-darwin" ];
    sourceProvenance = with pkgs.lib.sourceTypes; [ binaryNativeCode ];
  };
}
