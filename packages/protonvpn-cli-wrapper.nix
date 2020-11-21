{ coreutils, dialog, gnugrep, iproute, iptables, lib, makeWrapper, openvpn
, procps, protonvpn-cli, stdenv, unixtools, which, ... }:

with lib;
stdenv.mkDerivation {
  name = "protonvpn-cli-wrapper";

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [
    protonvpn-cli
    coreutils
    dialog
    gnugrep
    iproute
    iptables
    openvpn
    procps
    unixtools.ping
    which
  ];

  dontUnpack = true;

  installPhase = ''
    mkdir -p "''${out}/bin"
    cp "${protonvpn-cli}/bin/protonvpn" "''${out}/bin/protonvpn"

    wrapProgram "''${out}/bin/protonvpn" --prefix PATH : ${
      makeBinPath [
        coreutils
        dialog
        gnugrep
        iproute
        iptables
        openvpn
        procps
        unixtools.ping
        which
      ]
    }
  '';

  meta.platforms = platforms.linux;
}
