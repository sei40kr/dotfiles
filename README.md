# dotfiles 

[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [dotfiles](#dotfiles)
    - [Installation](#installation)
        - [Terminal emulator](#terminal-emulator)
        - [Add SSH key to GitHub account](#add-ssh-key-to-github-account)
        - [Clone dotfiles](#clone-dotfiles)
        - [Set up CLI environments](#set-up-cli-environments)
        - [Haskell IDE Engine](#haskell-ide-engine)
        - [Install fonts](#install-fonts)
        - [Install GUI applications](#install-gui-applications)
    - [TODO](#todo)

<!-- markdown-toc end -->

## Installation

### Terminal emulator

1. Install [Alacritty](https://github.com/jwilm/alacritty). It needs to build from source code.

1. Install `*.terminfo` to use 24bit colors and italic fonts.

    ```sh
    tic ~/dotfiles/terminfo/screen-256color-italic.terminfo
    tic ~/dotfiles/terminfo/xterm-256color-italic.terminfo
    ```

### Add SSH key to GitHub account

1. Generate new SSH key (ECDSA 256-bit)

    ```sh
    ssh-keygen -t ecdsa -b 256 -C "sei40kr@gmail.com"
    ```

1. Copy the key

    ```sh
    # macOS
    pbcopy <~/.ssh/id_ecdsa
    # Linux
    xclip -sel clip ~/.ssh/id_ecdsa
    ```

### Clone dotfiles

Clone this repository via SSH

```sh
git clone git@github.com:sei40kr/dotfiles.git ~/dotfiles
```

### Set up CLI environments

```sh
# Install Homebrew (macOS only)
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install Homebrew/Linuxbrew packages
brew bundle --file=~/dotfiles/etc/Brewfile

# Install Emacs and reattach-to-user-namespace (macOS only)
brew tap d12frosted/emacs-plus
brew install emacs-plus --with-24bit-color --with-natural-title-bar
brew install reattach-to-user-namespace

# Check the version of installed ZSH
which zsh | sudo tee -a /etc/shells >/dev/null
zsh --version

# Set default shell to ZSH
chsh -s "$(which zsh)"

# Install Haskell Tool Stack
curl -sSL https://get.haskellstack.org/ | sh
exec "$SHELL" -l

# Install Go packages
gotcha ~/dotfiles/etc/config.toml

# Install SDKMAN!
curl -s "https://get.sdkman.io" | bash
# Install Java and toolkits via SDKMAN!
sdk install java
sdk install kotlin
sdk install ant
sdk install maven
sdk install gradle

# Install Ruby, Node and packages via asdf
NODEJS_CHECK_SIGNATURES=no asdf install

# Install Python and packages via pyenv
pyenv global 2.7.14 3.6.4
pip2 install -r ~/dotfiles/etc/requirements.txt
pip3 install -r ~/dotfiles/etc/requirements3.txt
pyenv rehash
```

### Haskell IDE Engine

1. Install Haskell IDE Engine. It needs to build from source code.

    ```sh
    git clone https://github.com/haskell/haskell-ide-engine hie
    cd haskell-ide-engine
    stack install
    rm -rf hie
    ```

### Install fonts

1. Install Nerd fonts

    ```sh
    # macOS
    brew tap caskroom/fonts
    brew cask install font-hack-nerd-font
    # Linux
    git clone https://github.com/ryanoasis/nerd-fonts fonts --depth=1 &&
    ./fonts/install.sh &&
    rm -rf fonts
    ```

### Install GUI applications

1. Install these applicatons

* [Google Chrome](https://www.google.co.jp/chrome/browser/desktop)
* [Visual Studio Code](https://code.visualstudio.com/download)
* [Slack](https://slack.com/intl/ja-jp/downloads)

## TODO

* Create one-command installer
