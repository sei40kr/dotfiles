# spacemacs.bash --- Spacemacs installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_macos; then
    brew_tap_facade d12frosted/emacs-plus
    brew_install_facade emacs-plus
elif is_arch; then
    pacman_sync_facade emacs
fi

# TODO Clone the repo to ~/.emacs.d

ln_facade "${DOTFILES_PATH}/spacemacs" "${HOME}/.spacemacs.d"

# Ansible
pip2_install_facade ansible-lint

# C/C++
pip3_install_facade cmakelint
pip3_install_facade cpplint

# Docker
yarn_global_add_facade dockerfile-language-server-nodejs

# Go
go_get_facade github.com/cweill/gotests/...
go_get_facade github.com/davidrjenni/reftools/cmd/fillstruct
go_get_facade github.com/fatih/gomodifytags
go_get_facade github.com/godoctor/godoctor
go_get_facade github.com/haya14busa/gopkgs/cmd/gopkgs
go_get_facade github.com/josharian/impl
go_get_facade github.com/nsf/gocode
go_get_facade github.com/rogpeppe/godef
go_get_facade github.com/zmb3/gogetdoc
go_get_facade golang.org/x/tools/cmd/godoc
go_get_facade golang.org/x/tools/cmd/goimports
go_get_facade golang.org/x/tools/cmd/gorename
go_get_facade golang.org/x/tools/cmd/guru

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
if is_linux; then
    ln_facade "${DOTFILES_PATH}/matplotlib/matplotlibrc" \
              "${XDG_CONFIG_HOME}/matplotlib/matplotlibrc"
elif is_macos; then
    ln_facade "${DOTFILES_PATH}/matplotlib/matplotlibrc" \
              "${HOME}/.matplotlib/matplotlibrc"
fi

# Markdown
gem_install_facade mdl
yarn_global_add_facade markdownlint-cli
yarn_global_add_facade vmd

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

# Ruby
gem_install_facade fastri
gem_install_facade pry
gem_install_facade pry-coolline
gem_install_facade pry-doc
gem_install_facade rake
gem_install_facade rcodetools
gem_install_facade rubocop

# Shell-script
if is_macos; then
    brew_install_facade shfmt
elif is_arch; then
    trizen_sync_facade shfmt-bin
fi

# SQL
go_get_facade github.com/jackc/sqlfmt

# TypeScript
yarn_global_add_facade typescript-language-server

# Xclipboard
if is_arch; then
    pacman_sync_facade xsel
fi

# YAML
pip3_install_facade yamllint
