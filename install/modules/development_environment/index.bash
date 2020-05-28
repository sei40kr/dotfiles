# author: Seong Yong-ju <sei40kr@gmail.com>

# shellcheck source=editors.bash
# shellcheck source=programming_fonts.bash
# shellcheck source=tools.bash
# shellcheck source=language_tools/index.bash
# shellcheck source=infrastructure_cicd_tools.bash
# shellcheck source=competitive_programming_environments.bash
define_lazy_modules \
  editors install_editors \
  programming_fonts install_programming_fonts \
  tools install_tools \
  language_tools install_language_tools \
  infrastructure_cicd_tools install_infrastructure_cicd_tools \
  competitive_programming_environments install_competitive_programming_environments

install_development_environment() {
  while true; do
    tui_init_options
    tui_add_options \
      'Editors' install_editors \
      'Programming Fonts' install_programming_fonts \
      'Tools' install_tools \
      'Language Tools' install_language_tools \
      'Infrastructure & CI/CD Tools' install_infrastructure_cicd_tools \
      'Competitive Programming Environments' install_competitive_programming_environments
    tui_set_quit_option d 'Done'

    if ! tui_select_option 'Enter your option'; then
      break
    fi
  done
}
