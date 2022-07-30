{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.google-drive;

  package = pkgs.google-drive-ocamlfuse;
in
{
  options.modules.services.google-drive = with types; {
    enable = mkBoolOpt false;

    mountDir = mkOpt str "${config.user.home}/Google Drive";
  };

  config = mkIf cfg.enable {
    user.packages = [ package ];

    systemd.user.services.google-drive-ocamlfuse = {
      description = "FUSE filesystem over Google Drive";
      after = [ "network.target" ];
      wantedBy = [ "default.target" ];
      serviceConfig = {
        Type = "forking";
        Restart = "always";
      };
      script = ''
        ${package}/bin/google-drive-ocamlfuse ${escapeShellArg cfg.mountDir}
      '';
      preStart = ''
        mkdir -p ${escapeShellArg cfg.mountDir} 

        # Check the mount directory
        test -d ${escapeShellArg cfg.mountDir}
        test -w ${escapeShellArg cfg.mountDir}

        # Check the google-drive-ocamlfuse configuration
        test -d ~/.gdfuse/default
        test -w ~/.gdfuse/default
      '';
      preStop = ''
        fusermount -u ${escapeShellArg cfg.mountDir}
      '';
    };
  };
}
