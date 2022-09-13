{ runCommandLocal }:

let version = "unstable-2022-09-13";
in
runCommandLocal "sensible-terminal-${version}" { } ''
  mkdir -p $out/bin
  cp ${../../bin}/sensible-terminal $out/bin
  patchShebangs --build $out/bin/sensible-terminal
''
