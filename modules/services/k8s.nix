{ config, lib, ... }:

let
  inherit (lib)
    mdDoc
    member
    mkIf
    mkOption
    types
    ;
  inherit (types)
    bool
    enum
    listOf
    str
    ;
  cfg = config.modules.services.k8s;
in
{
  options.modules.services.k8s = {
    roles = mkOption {
      type = listOf (enum [
        "master"
        "node"
      ]);
      default = [ ];
      example = [
        "master"
        "node"
      ];
      description = mdDoc ''
        The roles of this node in the Kubernetes cluster. If empty, Kubernetes
        will not be enabled on this node.
      '';
    };

    masterHostname = mkOption {
      type = str;
      default = if (member "master" cfg.roles) then "localhost" else null;
      example = "k8s-master.local";
      description = mdDoc ''
        The hostname of the Kubernetes master node.
      '';
    };

    easyCerts = mkOption {
      type = bool;
      default = false;
      example = false;
      description = mdDoc ''
        Whether to use the easy-certs module to generate certificates for the
        Kubernetes cluster. Do not enable this for production clusters.
      '';
    };
  };

  config = mkIf (cfg.roles != [ ]) {
    services.kubernetes = {
      inherit (cfg) easyCerts roles;
      masterAddress = cfg.masterHostname;
    };
  };
}
