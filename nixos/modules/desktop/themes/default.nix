{ lib, ... }:

with lib;
with lib.my; {
  options.modules.desktop.theme = {
    active = mkOpt (types.enum [ "graphite" "material-design" "orchis" "whitesur" ]) null;
  };
}
