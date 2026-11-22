{ pkgs }:

pkgs.buildNpmPackage {
  pname = "quint-language-server";
  version = "0.19.0";

  # The server lives in the Quint monorepo with its own lockfile.
  inherit (pkgs.quint) src;
  sourceRoot = "source/vscode/quint-vscode/server";

  npmDepsHash = "sha256-BT5KN9E5aRUIJQR0zGlT00tDmRpS2oFF/yOdQOPIGgQ=";

  # Upstream imports a type from the VS Code client package, which only resolves in the extension's tree.
  postPatch = ''
    substituteInPlace src/complete.ts \
      --replace-fail "from 'vscode-languageclient'" "from 'vscode-languageserver'"
  '';

  npmBuildScript = "compile";

  meta = {
    description = "Language server for the Quint specification language";
    homepage = "https://github.com/informalsystems/quint";
    license = pkgs.lib.licenses.asl20;
    mainProgram = "quint-language-server";
  };
}
