{ lib, fetchurl, stdenv, undmg }:

stdenv.mkDerivation {
  pname = "amphetamine-enhancer";
  version = "unstable-2020-01-31";

  src = fetchurl {
    url = "https://raw.githubusercontent.com/x74353/Amphetamine-Enhancer/c372712a3127fd6e6d77b0bc727dff332489909f/Releases/Current/Amphetamine%20Enhancer.dmg";
    hash = "sha256-qISMBy46rm+J+smftPcbrJy8lrWynMV/fgEjXD45oUw=";
  };

  nativeBuildInputs = [ undmg ];

  sourceRoot = "Amphetamine Enhancer.app";

  postPatch = ''
    chmod +x Contents/Resources/amphetamine-enhancer-allProcesses.sh
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/Applications/Amphetamine\ Enhancer.app
    cp -R . $out/Applications/Amphetamine\ Enhancer.app

    for file in $out/Applications/Amphetamine\ Enhancer.app/Contents/Resources/*.plist; do
      substituteInPlace "$file" \
        --replace /Applications/Amphetamine\ Enhancer.app $out/Applications/Amphetamine\ Enhancer.app
    done

    substituteInPlace $out/Applications/Amphetamine\ Enhancer.app/Contents/Resources/amphetamine-enhancer-cdmManager.sh \
      --replace '/Applications/Amphetamine\ Enhancer.app' "$out/Applications/Amphetamine\\ Enhancer.app"

    runHook postInstall
  '';

  meta = with lib; {
    description = "Add new abilities to the macOS keep-awake utility, Amphetamine.";
    license = licenses.mit;
    platforms = platforms.darwin;
  };
}
