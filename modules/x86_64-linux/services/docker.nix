{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.docker;
in {
  options.modules.services.docker = {
    enable = mkBoolOpt false;
    autoPrune.enable = mkBoolOpt false;
    compose.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ docker-compose ];
    # TODO Use user Docker service
    virtualisation.docker = {
      enable = true;
      autoPrune.enable = cfg.autoPrune.enable;
    };
    modules.shell.zsh = {
      aliases = {
        di = "docker info";
        dlg = "docker container logs";
        dls = "docker container ls";
        dlsa = "docker container ls -a";
        dr = "docker container run";
        dt = "docker top";
        dv = "docker version";
        dpo = "docker container port";
        dpu = "docker pull";
        dx = "docker container exec";
        dbl = "docker build";
        dhh = "docker help";
        dpsa = "docker container ps -a";
        dils = "docker image ls";
        dit = "docker image tag";
        dip = "docker image push";
        dib = "docker image build";
        dnls = "docker network ls";
        dnc = "docker network create";
        dncn = "docker network connect";
        dndcn = "docker network disconnect";
        dnrm = "docker network rm";
        dvls = "docker volume ls";
        dvclean = "docker volume rm $(docker volume ls -qf dangling=true)";
        drmi =
          "docker rmi -f $(docker images -aq --filter dangling=true) 2>/dev/null";
        dwipe =
          "docker kill $(docker ps -q) 2>/dev/null;docker rm $(docker ps -aq) 2>/dev/null;docker rmi -f $(docker images -aq) 2>/dev/null";
      };
      zinitPluginsInit = ''
        zinit snippet OMZP::docker-compose/docker-compose.plugin.zsh
      '';
    };
  };
}
