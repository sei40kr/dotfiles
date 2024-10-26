{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.tools.k8s;
in
{
  options.modules.dev.tools.k8s = {
    enable = mkBoolOpt false;

    helm.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      kubectl
      kubectx
      stern
      (mkIf cfg.helm.enable kubernetes-helm)
    ];
  };
}
