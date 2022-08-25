{ runCommandLocal }:

let version = "unstable-2022-08-25";
in
runCommandLocal "wayland-sensible-terminal-${version}" { } ''
  mkdir -p $out/bin
  cp ${../../bin}/wayland-sensible-terminal $out/bin
  patchShebangs --build $out/bin/wayland-sensible-terminal
''
