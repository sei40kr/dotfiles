# author: Seong Yong-ju <sei40kr@gmail.com>

# brew_install FORMULA [OPTION ...] ...
#
# Install Homebrew formulas.
#
# Examples:
# brew_install zsh tmux
# brew_install \
#     d12frosted/emacs-plus --with-emacs-27-branch --without-spacemacs-icon \
#     libvterm cmake
#
brew_install() {
  assert_macos
  assert_command_exists brew

  local formula
  # Install options with double quotes
  local -a options

  function print_brewfile_line() {
    local IFS=','
    echo "brew \"${formula}\"${options:+, args: [${options[*]}]}"
  }

  {
    for arg in "$@"; do
      if [[ "$arg" == --* ]]; then
        options+=("\"${arg#--}\"")
      else
        if [[ -n "$formula" ]]; then
          print_brewfile_line
        fi

        formula="$arg"
        options=()
      fi
    done

    if [[ -n "$formula" ]]; then
      print_brewfile_line
    fi
  } | run_process brew bundle --file=-
}

# brew_cask_install CASK ...
#
# Install Homebrew casks.
#
# Example:
# brew_cask_install google-chrome
#
brew_cask_install() {
  assert_macos
  assert_command_exists brew

  {
    echo 'cask_args appdir: "/Applications"'

    for cask in "$@"; do
      echo "cask \"${cask}\""
    done
  } | run_process brew bundle --file=-
}
