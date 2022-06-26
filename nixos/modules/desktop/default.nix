{ lib, ... }:

with lib;
with lib.my;
{
  options.modules.desktop = {
    enable = mkBoolOpt false;
  };
}
