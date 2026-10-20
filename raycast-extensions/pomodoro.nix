{ pkgs }:

let
  rev = "6056e55c10f230ce21068b1a822a03cdf16d30e3";
in
pkgs.buildNpmPackage {
  # Must match the manifest name; the Tinycast module installs under it.
  pname = "pomodoro";
  version = "0-unstable-${builtins.substring 0 7 rev}";

  src = pkgs.fetchFromGitHub {
    owner = "raycast";
    repo = "extensions";
    inherit rev;
    sparseCheckout = [ "extensions/pomodoro" ];
    hash = "sha256-QvHDpWm4qozk/9vKK467kfzIoIanNMgahf74weMNTyk=";
  };
  sourceRoot = "source/extensions/pomodoro";

  # Upstream's lockfile omits node-fetch's tarball URL, so the npm deps fetcher skips it.
  postPatch = ''
    sed -i '/"node_modules\/node-fetch": {/a\      "resolved": "https://registry.npmjs.org/node-fetch/-/node-fetch-3.3.2.tgz",' package-lock.json
  '';

  npmDepsHash = "sha256-ck6OspxGbTzP6riA5hiEQSut1odlYUVls3by3DaS1a8=";

  # Same invocation as Tinycast's own source install.
  buildPhase = ''
    runHook preBuild
    node_modules/.bin/ray build -e dist -o "$out" --non-interactive
    runHook postBuild
  '';
  dontNpmInstall = true;
  installPhase = "true";

  meta = {
    description = "Raycast extension with a menu-bar Pomodoro timer";
    homepage = "https://www.raycast.com/asubbotin/pomodoro";
    license = pkgs.lib.licenses.mit;
  };
}
