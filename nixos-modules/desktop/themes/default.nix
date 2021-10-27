{ lib, ... }:

with lib;
with lib.my; {
  options.modules.desktop.themes.active =
    mkOpt (types.enum [ "material-design" ]) null;
}
