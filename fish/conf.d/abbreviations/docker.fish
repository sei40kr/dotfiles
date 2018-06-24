# docker.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# docker

abbr -a dk docker
abbr -a dka docker attach
abbr -a dkb docker build
abbr -a dkd docker diff
abbr -a dkdf docker system df
abbr -a dke docker exec
abbr -a dkE docker exec -it
abbr -a dkh docker history
abbr -a dki docker images
abbr -a dkin docker inspect
abbr -a dkim docker import
abbr -a dkk docker kill
abbr -a dkl docker logs
abbr -a dkli docker login
abbr -a dklo docker logout
abbr -a dkls docker ps
abbr -a dkp docker pause
abbr -a dkP docker unpause
abbr -a dkpl docker pull
abbr -a dkph docker push
abbr -a dkps docker ps
abbr -a dkpsa docker ps -a
abbr -a dkr docker run
abbr -a dkR docker run -it --rm
abbr -a dkRe docker run -it --rm --entrypoint /bin/bash
abbr -a dkRM docker system prune
abbr -a dkrm docker rm
abbr -a dkrmi docker rmi
abbr -a dkrn docker rename
abbr -a dks docker start
abbr -a dkS docker restart
abbr -a dkss docker stats
abbr -a dksv docker save
abbr -a dkt docker tag
abbr -a dktop docker top
abbr -a dkup docker update
abbr -a dkV docker volume
abbr -a dkv docker version
abbr -a dkw docker wait
abbr -a dkx docker stop

abbr -a dkC docker container
abbr -a dkCa docker container attach
abbr -a dkCcp docker container cp
abbr -a dkCd docker container diff
abbr -a dkCe docker container exec
abbr -a dkCin docker container inspect
abbr -a dkCk docker container kill
abbr -a dkCl docker container logs
abbr -a dkCls docker container ls
abbr -a dkCp docker container pause
abbr -a dkCpr docker container prune
abbr -a dkCrn docker container rename
abbr -a dkCS docker container restart
abbr -a dkCrm docker container rm
abbr -a dkCr docker container run
abbr -a dkCR docker container run -it --rm
abbr -a dkCRe docker container run -it --rm --entrypoint /bin/bash
abbr -a dkCs docker container start
abbr -a dkCss docker container stats
abbr -a dkCx docker container stop
abbr -a dkCtop docker container top
abbr -a dkCP docker container unpause
abbr -a dkCup docker container update
abbr -a dkCw docker container wait

abbr -a dkI docker image
abbr -a dkIb docker image build
abbr -a dkIh docker image history
abbr -a dkIim docker image import
abbr -a dkIin docker image inspect
abbr -a dkIls docker image ls
abbr -a dkIpr docker image prune
abbr -a dkIpl docker image pull
abbr -a dkIph docker image push
abbr -a dkIrm docker image rm
abbr -a dkIsv docker image save
abbr -a dkIt docker image tag

abbr -a dkV docker volume
abbr -a dkVin docker volume inspect
abbr -a dkVls docker volume ls
abbr -a dkVpr docker volume prune
abbr -a dkVrm docker volume rm

abbr -a dkN docker network
abbr -a dkNs docker network connect
abbr -a dkNx docker network disconnect
abbr -a dkNin docker network inspect
abbr -a dkNls docker network ls
abbr -a dkNpr docker network prune
abbr -a dkNrm docker network rm

abbr -a dkY docker system
abbr -a dkYdf docker system df
abbr -a dkYpr docker system prune

abbr -a dkK docker stack
abbr -a dkKls docker stack ls
abbr -a dkKps docker stack ps
abbr -a dkKrm docker stack rm

abbr -a dkW docker swarm

abbr -a dkrmC docker rm \(docker ps -qaf status=exited\)
abbr -a dkrmI docker rmi \(docker images -qf dangling=true\)
abbr -a dkrmV docker volume rm \(docker volume ls -qf dangling=true\)


# docker-compose

abbr -a dkc docker-compose
abbr -a dkcb docker-compose build
abbr -a dkcB docker-compose build --no-cache
abbr -a dkcd docker-compose down
abbr -a dkce docker-compose exec
abbr -a dkck docker-compose kill
abbr -a dkcl docker-compose logs
abbr -a dkcls docker-compose ps
abbr -a dkcp docker-compose pause
abbr -a dkcP docker-compose unpause
abbr -a dkcpl docker-compose pull
abbr -a dkcph docker-compose push
abbr -a dkcps docker-compose ps
abbr -a dkcr docker-compose run
abbr -a dkcR docker-compose run --rm
abbr -a dkcrm docker-compose rm
abbr -a dkcs docker-compose start
abbr -a dkcsc docker-compose scale
abbr -a dkcS docker-compose restart
abbr -a dkcu docker-compose up
abbr -a dkcU docker-compose up -d
abbr -a dkcv docker-compose version
abbr -a dkcx docker-compose stop
