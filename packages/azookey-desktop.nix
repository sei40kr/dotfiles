{ pkgs }:

let
  version = "0.1.5";
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "azookey-desktop";
  inherit version;

  src = pkgs.fetchurl {
    url = "https://github.com/azooKey/azooKey-Desktop/releases/download/v${version}/azooKey-release-signed.pkg";
    hash = "sha256-yMZMe30w55y2P/+OGVMZICYqS7w7rNT1LSoUK/EOMOU=";
  };

  nativeBuildInputs = [
    pkgs.xar
    pkgs.cpio
  ];

  # The pkg's only script removes a LaunchAgent left by pre-0.1 releases, so
  # the payload is all we need.
  unpackPhase = ''
    runHook preUnpack

    xar -xf "$src"
    gunzip -c azooKey-tmp.pkg/Payload | cpio -idm --quiet

    runHook postUnpack
  '';

  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  # Keep the bundle byte-identical so its code signature stays valid.
  dontFixup = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/Library/Input Methods"
    cp -R azooKeyMac.app "$out/Library/Input Methods/"

    runHook postInstall
  '';

  meta = {
    description = "Japanese input method for macOS powered by azooKey and Zenzai";
    homepage = "https://github.com/azooKey/azooKey-Desktop";
    license = pkgs.lib.licenses.mit;
    platforms = [
      "aarch64-darwin"
      "x86_64-darwin"
    ];
    sourceProvenance = with pkgs.lib.sourceTypes; [ binaryNativeCode ];
  };
}
