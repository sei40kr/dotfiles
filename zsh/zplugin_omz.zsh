# zplugin_omz.zsh --- ZSH plugins from Oh My Zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

() {
  local plugins=(
    # ZSH
    fancy-ctrl-z
    # Git
    git
    # Go
    golang
    # Java
    mvn
    # Node
    npm
    yarn
    react-native
    # Python
    autopep8
    pylint
    pip
    # Ruby
    capistrano
    gem
    rake
    # Scala
    sbt
    # Database & other middlewares
    postgres
    # Tools
    colored-man-pages
    extract
    nmap
  )

  if [[ "${+commands[aptitude]}" == 1 ]] \
         || [[ "${+commands[apt-get]}" == 1 ]]; then
    plugins=( debian $plugins )
  fi

  for plugin in $plugins; do
    zplugin ice svn
    zplugin snippet OMZ::"plugins/${plugin}"
  done

  local snippets=()

  for snippet in $snippets; do
    zplugin snippet OMZ::"plugins/${snippet}/${snippet}.plugin.zsh"
  done

  local completions=(
    # Haskell
    stack
    # Java
    ant
    gradle
    # Ruby
    rake-fast
    # Tools
    mosh
  )

  for completion in $completions; do
    zplugin ice svn wait''
    zplugin snippet OMZ::"plugins/${completion}"
  done
}
