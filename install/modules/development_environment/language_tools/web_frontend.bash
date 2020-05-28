# author: Seong Yong-ju <sei40kr@gmail.com>

install_web_frontend_language_tools() {
  if is_macos; then
    brew_install node yarn
  elif is_archlinux; then
    pacman_sync nodejs npm yarn
  else
    unsupported_platform_error
  fi

  git_clone nvm-sh/nvm "$NVM_DIR"
  (
    cd "$NVM_DIR"
    git fetch
    git checkout "$(git tag --sort=version:refname | tail -n1)"
  )

  (
    . "${NVM_DIR}/nvm.sh"
    nvm alias default system
  )

  yarn_global_add system \
    typescript \
    gulp \
    webpack-cli \
    stylelint-cli \
    eslint-cli \
    eslint_d \
    tslint \
    prettier \
    prettier-eslint-cli \
    typescript-formatter \
    vscode-html-languageserver-bin \
    vscode-css-languageserver-bin \
    javascript-typescript-langserver \
    typescript-language-server \
    vue-language-server
}
