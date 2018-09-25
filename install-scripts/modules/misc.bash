# misc.bash
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_macos; then
    brew_install_facade fd
    brew_install_facade jq

    brew_tap_facade tavianator/tap
    brew_install_facade bfs
elif is_arch; then
    pacman_sync_facade fd
    pacman_sync_facade jq

    trizen_sync_facade bfs
fi

pip3_install_facade asciinema
pip3_install_facade aws-shell

yarn_global_add_facade create-react-app
yarn_global_add_facade create-react-native-app
yarn_global_add_facade flow-bin
yarn_global_add_facade gatsby-cli
yarn_global_add_facade generate
yarn_global_add_facade generate-editorconfig
yarn_global_add_facade generate-gitignore
yarn_global_add_facade generate-license
yarn_global_add_facade generate-project
yarn_global_add_facade gulp
yarn_global_add_facade typescript
yarn_global_add_facade webpack
yarn_global_add_facade yo
