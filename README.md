# dotfiles

## Installation

### Install fonts

1. Install Powerline fonts

    ```sh
    # clone
    git clone https://github.com/powerline/fonts.git --depth=1
    # install
    cd fonts
    ./install.sh
    # clean-up a bit
    cd ..
    rm -rf fonts
    ```

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

### macOS: Install Homebrew

1. Install Homebrew

    ```sh
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    ```

### Linux: Install Linuxbrew

1. Install Linuxbrew

    ```sh
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
    ```

### Add SSH key to GitHub account

1. Install OpenSSL via Homebrew.

    ```sh
    brew install openssl
    ```

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

    and add it to [GitHub SSH keys](https://github.com/settings/keys) of the account.

### Clone dotfiles

1. Clone this repository via SSH

    ```sh
    git clone git@github.com:sei40kr/dotfiles.git ~/dotfiles
    ```

### Set up terminal emulator

1. Install terminal emulator

    * [iTerm2](https://www.iterm2.com)
    * [Alacritty](https://github.com/jwilm/alacritty)

1. Install terminal colorscheme

    * [One Dark](https://github.com/joshdick/onedark.vim)
    * [Iceberg](http://cocopon.github.io/iceberg.vim)
    * [Oceanic Next (for iTerm2)](https://github.com/mhartington/oceanic-next-iterm)
    * [Oceanic Next (for GNOME Terminal)](https://github.com/denysdovhan/oceanic-next-gnome-terminal)
    * [Snazzy (for iTerm2)](https://github.com/sindresorhus/iterm2-snazzy)

1. Install terminal profiles

    ```sh
    tic ~/dotfiles/terminfo/screen-256color-italic.terminfo
    tic ~/dotfiles/terminfo/xterm-256color-italic.terminfo
    ```

### Install ZSH

1. Install ZSH via brew command

    ```sh
    brew install zsh
    ```

1. Check the latest version of ZSH is successfully installed

    ```sh
    # /path/to/.linuxbrew/bin/zsh
    which zsh
    zsh --version
    ```

    and set it as default shell

    ```sh
    which zsh | sudo tee -a /etc/shells >/dev/null
    chsh -s "$(which zsh)"
    ```

1. Install zplugin (plugin manager & loader for ZSH)

    ```sh
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
    ```

1. Create symlinks

    ```sh
    ln -sf ~/dotfiles/.zsh ~/.zsh
    ln -sf ~/dotfiles/.zshenv ~/.zshenv
    ln -sf ~/dotfiles/.zshrc ~/.zshrc
    ```

### Install TMUX

1. Install TMUX via brew command

    ```sh
    brew install tmux
    # macOS
    brew install reattach-to-user-namespace
    ```

1. Install TPM

    ```sh
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    ```

1. Create symlink

    ```sh
    ln -sf ~/dotfiles/.tmux.conf ~/.tmux.conf
    ```

### Install Git

1. Install the latest version of Git via brew command

    ```sh
    brew install git diff-so-fancy
    ```

1. Create symlink

    ```sh
    ln -sf ~/dotfiles/.gitconfig ~/.gitconfig
    ```

### Install tig

1. Install tig via brew command

    ```sh
    brew install tig
    ```

1. Create symlinks

    ```sh
    ln -sf ~/dotfiles/.tig ~/.tig
    ln -sf ~/dotfiles/.tigrc ~/.tigrc
    ```

### Install version managers

1. Install anyenv (manager for version managers)

    ```sh
    git clone https://github.com/riywo/anyenv ~/.anyenv
    ```

1. Install SDKMAN!

    ```sh
    curl -s "https://get.sdkman.io" | bash
    ```

1. Install Haskell Tool Stack

    ```sh
    curl -sSL https://get.haskellstack.org/ | sh
    ```

1. Reload the shell

    ```sh
    exec "$SHELL" -l
    ```

1. Install version managers via anyenv

    ```sh
    anyenv install goenv
    anyenv install pyenv
    anyenv install rbenv
    anyenv install crenv
    anyenv install ndenv
    ```

1. Reload the shell again

    ```sh
    exec "$SHELL" -l
    ```

### Set up Haskell

1. Install Haskell IDE Engine

    ```sh
    git clone https://github.com/haskell/haskell-ide-engine hie
    cd haskell-ide-engine
    stack install
    rm -rf hie
    ```

### Set up Go

1. Install the latest version of Go (check versions via `goenv install -l`)

    ```sh
    goenv install 1.9.2 && goenv global 1.9.2
    ```

### Set up Java

1. Install Java

    ```sh
    sdk install java
    ```

1. Install command-line tools for Java (& Kotlin) development

    ```sh
    sdk install kotlin
    sdk install ant
    sdk install maven
    sdk install gradle
    ```

### Set up Python

1. Install the latest versions of Python 2 and 3
    (check versions via `pyenv install -l`)

    ```sh
    pyenv install 3.6.4 &&
        pyenv install 2.7.14 &&
        pyenv global 3.6.4 2.7.14
    ```

### Set up Ruby

1. Install the latest version of Ruby (check versions via `rbenv install -l`)

    ```sh
    rbenv install 2.5.0 && rbenv global 2.5.0
    ```

1. Create symlink

    ```sh
    ln -sf ~/dotfiles/.gemrc ~/.gemrc
    ```

### Set up Node.js

1. Install the latest version of Node.js (check versions via `ndenv install -l`)

    ```sh
    ndenv install v9.3.0 && ndenv global v9.3.0
    ```

1. Install Yarn

    ```sh
    brew install yarn --without-node
    ```

### Install global

1. Install global via brew command

    ```sh
    brew install global
    ```

1. Install Pygments

    ```sh
    pip install pygments
    pyenv rehash
    ```

1. Create symlink

    ```sh
    ln -sf ~/dotfiles/.globalrc ~/.globalrc
    ```

### Install Vim and Neovim

1. Install Vim and Neovim via brew command

    ```sh
    brew install vim neovim
    ```

1. Install neovim packages

    ```sh
    pip2 install neovim
    pip3 install neovim
    pyenv rehash
    gem install neovim
    rbenv rehash
    ```

### Install SpaceVim

1. Install SpaceVim

    ```sh
    curl -sLf https://spacevim.org/install.sh | bash
    ```

1. Create symlink

    ```sh
    ln -sf ~/dotfiles/.SpaceVim.d ~/.SpaceVim.d
    ```

### Install Emacs

1. Install Emacs via brew command

    ```sh
    # macOS
    brew tap d12frosted/emacs-plus
    brew install emacs-plus --with-natural-title-bars
    # Linux
    brew install emacs
    ```

### Install Spacemacs

1. Install Spacemacs

    ```sh
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
    ```

1. Create symlink

    ```sh
    ln -sf ~/dotfiles/.spacemacs.d ~/.spacemacs.d
    ```

### Install other command-line tools

1. Install other command-line tools via brew command

    ```sh
    brew bundle --file=~/dotfiles/Brewfile
    ```

1. Install gotcha

    ```sh
    brew install gotcha
    ```

1. Install command-line tools via gotcha

    ```sh
    gotcha ~/dotfiles/config.toml
    ```

1. Install Python dependencies

    ```sh
    pip2 install -r ~/dotfiles/requirements.txt
    pip3 install -r ~/dotfiles/requirements3.txt
    pyenv rehash
    ```

1. Install Bundler

    ```sh
    gem install bundler
    rbenv rehash
    ```

1. Install Rubygems of command-line tools via Bundler

    ```sh
    bundler install --gemfile=~/dotfiles/Gemfile
    rbenv rehash
    ```

1. Install Node packages of command-line tools globally

    ```sh
    yarn global add \
        create-react-app \
        create-react-native-app \
        dockerfile-language-server-nodejs \
        eslint-cli \
        eslint_d \
        flow-bin \
        flow-language-server \
        gatsby-cli \
        generate \
        generate-editorconfig \
        generate-gitignore \
        generate-license \
        generate-project \
        gulp \
        import-js \
        prettier \
        prettier_d \
        prettier-eslint-cli \
        stylefmt \
        stylelint-cli \
        stylelint_d \
        typescript \
        typescript-language-server \
        webpack \
        vscode-css-languageserver-bin \
        vscode-html-languageserver-bin \
        vscode-json-languageserver-bin \
        yaml-language-server
    ndenv rehash
    ```

1. Create symlinks

    ```sh
    ln -sf ~/dotfiles/.jira.d ~/.jira.d
    ln -sf ~/dotfiles/.prettierrc.js ~/.prettierrc.js
    ln -sf ~/dotfiles/.prettierignore ~/.prettierignore
    ```

1. Copy `.importjs.js` (creating symlink doesn't work)

    ```sh
    cp -f ~/dotfiles/.importjs.js ~/.importjs.js
    ```

### Install GUI applications

1. Install these applicatons

    * [Google Chrome](https://www.google.co.jp/chrome/browser/desktop)
    * [Visual Studio Code](https://code.visualstudio.com/download)
    * [Slack](https://slack.com/intl/ja-jp/downloads)

## TODO

* Create one-command installer
