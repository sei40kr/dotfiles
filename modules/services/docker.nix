{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.services.docker;
in
{
  options.modules.services.docker = {
    compose.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      docker
      (mkIf cfg.compose.enable docker-compose)
    ];

    users.groups.docker = { };

    boot.kernelModules = [
      "bridge"
      "veth"
    ];
    boot.kernel.sysctl = {
      "net.ipv4.conf.all.forwarding" = mkOverride 98 true;
      "net.ipv4.conf.default.forwarding" = mkOverride 98 true;
    };

    systemd.services.docker = {
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "docker.socket"
      ];
      requires = [ "docker.socket" ];
      serviceConfig = {
        Type = "notify";
        ExecStart = "${pkgs.docker}/bin/dockerd";
        ExecReload = "${pkgs.procps}/bin/kill -s HUP $MAINPID";
      };
      path = [ pkgs.kmod ];
    };
    systemd.sockets.docker = {
      description = "Docker Socket for the API";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "/run/docker.sock";
        SocketMode = "0660";
        SocketUser = "root";
        SocketGroup = "docker";
      };
    };

    user.extraGroups = [ "docker" ];
  };
}
