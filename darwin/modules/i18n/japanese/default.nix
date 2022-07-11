{ lib, ... }:

with lib;
with lib.my;
{
  options.modules.i18n.japanese = {
    enable = mkBoolOpt false;
  };
}
