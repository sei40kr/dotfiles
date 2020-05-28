# author: Seong Yong-ju

declare core__current_module
declare core__module_basepath
core__module_basepath="$(cd "$(dirname "${BASH_SOURCE[0]}")/../modules" && pwd)"

# use_lazy_modules (MODULE TRIGGER-FUNCTION) ...
#
use_lazy_modules() {
  while [[ "$#" -gt 0 ]]; do
    local module="$1"
    shift
    local trigger_function="$1"
    shift

    assert_not_empty "$module"
    assert_not_empty "$trigger_function"

    core__use_lazy_module "$module" "$trigger_function"
  done
}

# core__use_lazy_module MODULE TRIGGER-FUNCTION
#
core__use_lazy_module() {
  local module="$1"
  local trigger_function="$2"

  assert_not_empty "$module"
  assert_not_empty "$trigger_function"

  local module_abstract="${core__current_module:+${core__current_module}/}${module}"

  local file
  if [[ -f "${core__module_basepath}/${module_abstract}.bash" ]]; then
    file="${core__module_basepath}/${module_abstract}.bash"
  elif [[ -d "${core__module_basepath}/${module_abstract}" && -f "${core__module_basepath}/${module_abstract}/index.bash" ]]; then
    file="${core__module_basepath}/${module_abstract}/index.bash"
  else
    tui-error "Module \"${module_abstract}\" is not found. Aborting."
  fi

  eval "function ${trigger_function}() {
    (
      core__current_module=$(printf '%q' "$module_abstract")
      . $(printf '%q' "$file")

      eval ${trigger_function}
    )
  }"
}

# abbreviate_filepath
#
# Abbreviate a file path.
#
# Examples:
# abbreviate_filepath "${HOME}/.emacs.d"    # prints ~/.emacs.d
# abbreviate_filepath /usr/bin/emacs        # prints /usr/bin/emacs
#
abbreviate_filepath() {
  local filepath="$1"

  # Make sure $HOME is at the beginning
  if [[ "$filepath" == "$HOME"* ]]; then
    filepath="${filepath/${HOME}/~}"
  fi

  echo "$filepath"
}
