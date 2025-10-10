{ lib, ... }:

let
  inherit (lib.types) enum;
  inherit (lib.my) mkOpt;
in
{
  imports = [
    ./graphite
    ./orchis
    ./whitesur
  ];

  options.modules.desktop.theme = {
    active = mkOpt (enum [
      "graphite"
      "orchis"
      "whitesur"
    ]) null;
  };
}
