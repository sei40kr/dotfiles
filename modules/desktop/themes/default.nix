{ lib, ... }:

with lib;
with lib.my;
{
  imports = [
    ./graphite
    ./orchis
    ./whitesur
  ];

  options.modules.desktop.theme = {
    active = mkOpt (types.enum [
      "graphite"
      "orchis"
      "whitesur"
    ]) null;
  };
}
