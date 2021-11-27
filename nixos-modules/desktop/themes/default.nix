{ lib, ... }:

with lib;
with lib.my; {
  options.modules.desktop.theme = {
    active = mkOpt (types.enum [ "material-design" "orchis" ]) null;
  };
}
