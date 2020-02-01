# dotfiles

## Requirements

- Arch Linux or macOS
- Git

## Install

Clone this repository and run the installer:

```sh
git clone git@github.com:sei40kr/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
./install
```

Make sure not to track the files with secret values:

``` sh
git update-index --assume-unchanged zsh/secrets.zsh
```
