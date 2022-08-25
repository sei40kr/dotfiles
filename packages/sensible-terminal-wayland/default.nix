{ runCommandLocal }:

let version = "unstable-2022-08-25";
in
runCommandLocal "sensible-terminal-wayland-${version}" { } ''
  mkdir -p $out/bin
  cp ${../../bin}/sensible-terminal-wayland $out/bin
  patchShebangs --build $out/bin/sensible-terminal-wayland
''
