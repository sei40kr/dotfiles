# spacemacs.bash --- Spacemacs installer
# author: Seong Yong-ju <sei40kr@gmail.com>

pacman_sync_facade emacs

# TODO Clone the repo to ~/.emacs.d

ln_facade "${DOTFILES_PATH}/spacemacs" "${HOME}/.spacemacs.d"

# Ansible
pip2_install_facade ansible-lint

# C/C++
pip3_install_facade cmakelint
pip3_install_facade cpplint

# Docker
yarn_global_add_facade dockerfile-language-server-nodejs

# HTML
yarn_global_add_facade stylelint-cli

# JavaScript
yarn_global_add_facade eslint-cli
yarn_global_add_facade eslint_d
yarn_global_add_facade import-js
yarn_global_add_facade prettier
yarn_global_add_facade prettier-eslint-cli
yarn_global_add_facade tern

# Jupyter Notebook
pip3_install_facade jupyter

# Python
pip3_install_facade autopep8
pip3_install_facade flake8
pip3_install_facade importmagic
pip3_install_facade jedi
pip3_install_facade pylint
pip3_install_facade pyls-isort
pip3_install_facade pyls-mypy
pip3_install_facade python-language-server
pip3_install_facade yapf

# Markdown
yarn_global_add_facade markdownlint-cli
yarn_global_add_facade vmd

# TypeScript
yarn_global_add_facade typescript-language-server

# YAML
pip3_install_facade yamllint
