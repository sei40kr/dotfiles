{ runCommandLocal }:

let version = "unstable-2022-08-25";
in
runCommandLocal "sensible-browser-${version}" { } ''
  mkdir -p $out/bin
  cp ${../../bin}/sensible-browser $out/bin
  patchShebangs --build $out/bin/sensible-browser
''
