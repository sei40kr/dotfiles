{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.docker;
in
{
  options.modules.services.docker = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    modules.shell.aliases = {
      # For docker
      dbl = "docker build";
      dcin = "docker container inspect";
      dcls = "docker container ls";
      dclsa = "docker container ls -a";
      dib = "docker image build";
      dii = "docker image inspect";
      dils = "docker image ls";
      dipu = "docker image push";
      dirm = "docker image rm";
      dit = "docker image tag";
      dlo = "docker container logs";
      dnc = "docker network create";
      dncn = "docker network connect";
      dndcn = "docker network disconnect";
      dni = "docker network inspect";
      dnls = "docker network ls";
      dnrm = "docker network rm";
      dpo = "docker container port";
      dpu = "docker pull";
      dr = "docker container run";
      drit = "docker container run -it";
      drm = "docker container rm";
      "drm!" = "docker container rm -f";
      dst = "docker container start";
      drs = "docker container restart";
      dsta = "docker stop $(docker ps -q)";
      dstp = "docker container stop";
      dtop = "docker top";
      dvi = "docker volume inspect";
      dvls = "docker volume ls";
      dvprune = "docker volume prune";
      dxc = "docker container exec";
      dxcit = "docker container exec -it";
    } // (optionalAttrs (pkgs.stdenv.isDarwin || cfg.compose.enable) {
      # For docker-compose
      dco = "docker-compose";
      dcb = "docker-compose build";
      dce = "docker-compose exec";
      dcps = "docker-compose ps";
      dcrestart = "docker-compose restart";
      dcrm = "docker-compose rm";
      dcr = "docker-compose run";
      dcstop = "docker-compose stop";
      dcup = "docker-compose up";
      dcupb = "docker-compose up --build";
      dcupd = "docker-compose up -d";
      dcupdb = "docker-compose up -d --build";
      dcdn = "docker-compose down";
      dcl = "docker-compose logs";
      dclf = "docker-compose logs -f";
      dcpull = "docker-compose pull";
      dcstart = "docker-compose start";
      dck = "docker-compose kill";
    });
  };
}
