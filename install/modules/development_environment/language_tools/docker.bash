# author: Seong Yong-ju <sei40kr@gmail.com>

install_docker_language_tools() {
  if is_macos; then
    brew_cask_install docker
    brew_install docker-compose
  elif is_archlinux; then
    pacman_sync docker docker-compose
  else
    unsupported_platform_error
  fi

  yarn_global_add system dockerfile-language-server-nodejs
}
