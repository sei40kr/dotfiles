# author: Seong Yong-ju <sei40kr@gmail.com>

install_markdown_language_tools() {
  gem_install system mdl
  yarn_global_add system markdownlint-cli
  pip_install system grip
}
