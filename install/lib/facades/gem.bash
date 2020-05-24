# author: Seong Yong-ju <sei40kr@gmail.com>

# gem_install VERSION PACKAGE ...
#
# Install Rubygems to specified version of Ruby.
#
gem_install() {
  local ruby_version="$1"
  shift
  local -a gems=("$@")
  local -a gem_opts=(-q --silent --norc)

  local system_ruby
  local gem_exec
  if [[ "$ruby_version" == system ]]; then
    system_ruby=1

    if is_macos; then
      gem_exec=/usr/local/bin/gem
    else
      gem_exec=/usr/bin/gem
    fi

    assert_executable "$gem_exec" \
      'gem executable of system-installed version not found. Aborting.'

    gem_opts+=(--user-install)
  else
    gem_exec="${RBENV_ROOT}/versions/${ruby_version}/bin/gem"

    assert_executable "$gem_exec" \
      "gem executable of ${ruby_version} not found. Aborting."
  fi

  for gem in "${gems[@]}"; do
    print-list-item "Installing ${gem}${system_ruby:- to Ruby ${ruby_version}}"
  done

  run_process "$gem_exec" install "${gem_opts[@]}" "${gems[@]}"
}
