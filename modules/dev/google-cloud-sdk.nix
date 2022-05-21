{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.google-cloud-sdk;
in
{
  options.modules.dev.google-cloud-sdk = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ google-cloud-sdk ];
  };
}
