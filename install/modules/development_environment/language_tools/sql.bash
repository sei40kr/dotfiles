# author: Seong Yong-ju <sei40kr@gmail.com>

install_sql_language_tools() {
  pip_install system sqlint sqlparse mycli pgcli litecli

  ln -fs "${HOME}/.dotfiles/mycli/myclirc" "${HOME}/.myclirc"
  ln -fs "${HOME}/.dotfiles/pgcli/pgclirc" "${HOME}/.pgclirc"
}
