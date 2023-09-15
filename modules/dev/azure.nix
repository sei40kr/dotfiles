{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.azure;
in
{
  options.modules.dev.azure = {
    enable = mkBoolOpt false;

    kubelogin.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      azure-cli
      (mkIf cfg.kubelogin.enable kubelogin)
    ];
  };
}
