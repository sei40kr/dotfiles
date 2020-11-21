{ clipmenu, coreutils, gawk, lib, makeWrapper, pkgs, stdenv, xsel, ... }:

with lib; {
  clipmenu = stdenv.mkDerivation {
    pname = "clipmenu";
    version = "unstable-2020-11-21";

    src = ./clipmenu;

    nativeBuildInputs = [ makeWrapper ];
    buildInputs = [ coreutils gawk xsel ];

    installPhase = ''
      mkdir -p "''${out}/bin"
      cp clipmenu "''${out}/bin"

      wrapProgram "''${out}/bin/clipmenu" \
        --prefix PATH : ${makeBinPath [ coreutils gawk xsel ]} \
        --set CM_MAJOR_VERSION ${
          builtins.elemAt (splitVersion clipmenu.version) 0
        }
    '';
  };
}
