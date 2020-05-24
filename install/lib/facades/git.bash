# author: Seong Yong-ju <sei40kr@gmail.com>

shopt -s extglob
shopt -s nullglob

# git_clone REPOSITORY [BRANCH] DESTINATION
#
# Clone a Git repository.
#
# Examples:
# git_clone sei40kr/dotfiles ~/.dotfiles
# git_clone https://github.com/zdharma/zinit ~/.zinit/bin
# git_clone https://github.com/hlissner/doom-emacs develop ~/.emacs.d
#
git_clone() {
  assert_command_exists git

  local repository="$1"
  local branch
  shift
  if [[ "$#" -ge 2 ]]; then
    branch="$1"
    shift
  fi
  local destination="$1"
  local -a clone_options=()

  if [[ "$repository" == +([!/])/+([!/]) ]]; then
    repository="https://github.com/${repository}.git"
  fi

  if [[ -n "$branch" ]]; then
    clone_options+=(-b "$branch")
  fi

  if [[ -d "$destination" ]]; then
    local -a files=(${destination}/*)

    if [[ "${#files[@]}" != 0 ]]; then
      local origin_url="$(git -C "$destination" remote get-url origin 2>/dev/null)"

      # The origin's URL and the repository are not same -> error
      if [[ "$origin_url" != "$repository" ]]; then
        tui-error "$(abbreviate_filepath "$destination") already exists."
        exit 1
      fi

      exit
    fi
  fi

  git clone -q --recurse-submodules "$repository" "${clone_options[@]}" \
    "$destination"
}

# git_clone_build REPOSITORY [BRANCH] COMMAND
#
# Clone a Git repository into a temporary directory and run a command there.
#
# Example:
# assert_command_exists cargo
# git_clone_build rust-analyzer/rust-analyzer 'cargo xtask install --server'
#
git_clone_build() {
  assert_command_exists git

  local repository="$1"
  local branch
  shift
  if [[ "$#" -ge 2 ]]; then
    branch="$1"
    shift
  fi
  local command="$1"
  local destination
  destination="$(mktemp -d)"

  git_clone "$repository" "$branch" "$destination"
  (
    cd "$destination"
    eval "$command"
  )

  rm -rf "$destination"
}
