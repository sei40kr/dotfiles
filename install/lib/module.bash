# author: Seong Yong-ju <sei40kr@gmail.com>

declare module__current
declare module__basepath
module__basepath="$(cd "$(dirname "${BASH_SOURCE[0]}")/../modules" && pwd)"

# use_lazy_modules (MODULE TRIGGER-FUNCTION) ...
#
use_lazy_modules() {
  local module
  local trigger_function

  while [[ "$#" -gt 0 ]]; do
    module="$1"
    shift
    trigger_function="$1"
    shift

    assert_not_empty "$module"
    assert_not_empty "$trigger_function"

    module__use_lazy_module "$module" "$trigger_function"
  done
}

# module__use_lazy_module MODULE TRIGGER-FUNCTION
#
module__use_lazy_module() {
  local module="$1"
  local trigger_function="$2"

  assert_not_empty "$module"
  assert_not_empty "$trigger_function"

  local module_abstract="${module__current:+${module__current}/}${module}"

  local file
  if [[ -f "${module__basepath}/${module_abstract}.bash" ]]; then
    file="${module__basepath}/${module_abstract}.bash"
  elif [[ -d "${module__basepath}/${module_abstract}" && -f "${module__basepath}/${module_abstract}/index.bash" ]]; then
    file="${module__basepath}/${module_abstract}/index.bash"
  else
    tui-error "Module \"${module_abstract}\" is not found. Aborting."
  fi

  eval "function ${trigger_function}() {
    (
      module__current=$(printf '%q' "$module_abstract")
      . $(printf '%q' "$file")

      eval ${trigger_function}
    )
  }"
}
