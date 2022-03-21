{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.k8s;
in
{
  options.modules.dev.k8s = {
    enable = mkBoolOpt false;

    kind.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = !cfg.kind.enable || config.modules.services.docker.enable;
      message =
        "The k8s module requires 'modules.desktop.docker.enable = true'.";
    }];

    user.packages = with pkgs; [
      kubectl
      kubectx
      (mkIf cfg.kind.enable kind)
    ];
  };
}
