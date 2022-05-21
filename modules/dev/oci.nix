{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.oci;
in
{
  options.modules.dev.oci = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ oci-cli ];
  };
}
