{ pkgs }:

let
  sources = builtins.fromJSON (builtins.readFile ./sources.json);
  inherit (sources) version;
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "kotlin-lsp";
  inherit version;

  src = pkgs.fetchurl sources.${pkgs.stdenvNoCC.hostPlatform.system};

  # The .sit archive is a plain zip.
  nativeBuildInputs = [
    pkgs.unzip
    pkgs.makeWrapper
  ];
  unpackCmd = "unzip -q $curSrc";

  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  # Keep the bundled JBR and native launcher signatures intact.
  dontFixup = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/share/kotlin-lsp" "$out/bin"
    cp -R . "$out/share/kotlin-lsp"
    # The launcher locates the distribution from its own path, so exec it rather than symlink.
    makeWrapper "$out/share/kotlin-lsp/bin/intellij-server" "$out/bin/intellij-server"
    makeWrapper "$out/share/kotlin-lsp/bin/intellij-server" "$out/bin/kotlin-lsp"

    runHook postInstall
  '';

  meta = {
    description = "Kotlin language server by JetBrains";
    homepage = "https://github.com/Kotlin/kotlin-lsp";
    license = pkgs.lib.licenses.asl20;
    mainProgram = "intellij-server";
    platforms = builtins.filter (name: name != "version") (builtins.attrNames sources);
    sourceProvenance = with pkgs.lib.sourceTypes; [
      binaryNativeCode
      binaryBytecode
    ];
  };
}
