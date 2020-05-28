# author: Seong Yong-ju <sei40kr@gmail.com>

# shellcheck source=c.bash
# shellcheck source=rust.bash
# shellcheck source=go.bash
# shellcheck source=haskell.bash
# shellcheck source=java.bash
# shellcheck source=kotlin.bash
# shellcheck source=scala.bash
# shellcheck source=groovy.bash
# shellcheck source=python.bash
# shellcheck source=ruby.bash
# shellcheck source=perl.bash
# shellcheck source=web_frontend.bash
# shellcheck source=sql.bash
# shellcheck source=r.bash
# shellcheck source=shell_script.bash
# shellcheck source=markdown.bash
# shellcheck source=latex.bash
# shellcheck source=plantuml.bash
# shellcheck source=ansible.bash
# shellcheck source=docker.bash
define_lazy_modules \
  c install_c_language_tools \
  rust install_rust_dev_tools \
  go install_go_language_tools \
  haskell install_haskell_language_tools \
  java install_java_language_tools \
  kotlin install_kotlin_language_tools \
  scala install_scala_language_tools \
  groovy install_groovy_language_tools \
  python install_python_language_tools \
  ruby install_ruby_language_tools \
  perl install_python_language_tools \
  web_frontend install_web_frontend_language_tools \
  sql install_sql_language_tools \
  r install_r_language_tools \
  shell_script install_shell_script_language_tools \
  markdown install_markdown_language_tools \
  latex install_latex_language_tools \
  plantuml install_plantuml_language_tools \
  ansible install_ansible_language_tools \
  docker install_docker_language_tools

install_language_tools() {
  while true; do
    print_title 'Language Tools'

    tui_init_options
    tui_add_options \
      'C' install_c_language_tools \
      'Rust' install_rust_dev_tools \
      'Go' install_go_language_tools \
      'Haskell' install_haskell_language_tools \
      'Java' install_java_language_tools \
      'Kotlin (requires Java)' install_kotlin_language_tools \
      'Scala (requires Java)' install_scala_language_tools \
      'Groovy (requires Java)' install_groovy_language_tools \
      'Python' install_python_language_tools \
      'Ruby' install_ruby_language_tools \
      'Perl' install_python_language_tools \
      'Web Frontend' install_web_frontend_language_tools \
      'SQL' install_sql_language_tools \
      'R' install_r_language_tools \
      'Shell Scripts' install_shell_script_language_tools \
      'Markdown (requires Node.js)' install_markdown_language_tools \
      'LaTeX' install_latex_language_tools \
      'PlantUML (requires Java)' install_plantuml_language_tools \
      'Ansible (requires Python)' install_ansible_language_tools \
      'Docker' install_docker_language_tools
    tui_set_quit_option d 'Done'

    if ! tui_select_option 'Enter your option'; then
      break
    fi
  done
}
