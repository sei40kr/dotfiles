{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf optional;
  cfg = config.modules.dev.tools.k8s;
in
{
  options.modules.dev.tools.k8s = {
    enable = mkEnableOption "Kubernetes development environment";

    helm.enable = mkEnableOption "Helm";
  };

  config = mkIf cfg.enable {
    home.packages =
      with pkgs;
      [
        kubectl
        kubectx
        stern
      ]
      ++ optional cfg.helm.enable kubernetes-helm;
  };
}
