# author: Seong Yong-ju <sei40kr@gmail.com>

# shellcheck source=zsh.bash
# shellcheck source=tmux.bash
# shellcheck source=essential_tools.bash
# shellcheck source=system_tools.bash
use_lazy_modules \
  zsh install_zsh \
  tmux install_tmux \
  shell_tools install_shell_tools \
  system_tools install_system_tools

install_shell_environment() {
  while true; do
    tui_init_options
    tui_add_options \
      'zsh' install_zsh \
      'tmux' install_tmux \
      'Shell Tools' install_shell_tools \
      'System Tools' install_system_tools
    tui_set_quit_option d 'Done'

    if ! tui_select_option 'Enter your option'; then
      break
    fi
  done
}
