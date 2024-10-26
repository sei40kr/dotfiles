{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.tools.azure;
in
{
  options.modules.dev.tools.azure = {
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
