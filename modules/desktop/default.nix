{ lib, ... }:

with lib;
with lib.my; {
  options.modules.desktop = { wayland.enable = mkBoolOpt false; };
}
