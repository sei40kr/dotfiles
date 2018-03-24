#!/usr/bin/env bash

# install.bash
# Seong Yong-ju <sei40kr@gmail.com>

DOTFILES_DIR="${HOME}/dotfiles"

if ! command -v git 1>/dev/null; then
  echo 'Error: This installer requires Git command.' 1>&2
  exit 1
fi

if [[ ! -d "${DOTFILES_DIR}" ]]; then
  echo 'Info: dotfiles is not installed. Cloning the repo from remote.'
  git clone git@github.com:sei40kr/dotfiles.git "${DOTFILES_DIR}"
fi

cd "$DOTFILES_DIR"

find "${DOTFILES_DIR}/etc/setup" \
     -mindepth 1 \
     -maxdepth 1 \
     -type f \
     -name '*.bash' \
  | while read -r script_path; do
  if [[ -x "$script_path" ]]; then
    bash "${script_path}"
  fi
done
