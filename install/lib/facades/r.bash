# author: Seong Yong-ju <sei40kr@gmail.com>

# r_install PACKAGE ...
#
# Install R packages.
#
r_install() {
  assert_command_exists R

  local -a packages
  packages=("$@")

  for package in "${packages[@]}"; do
    print-list-item "Installing ${package}"
  done

  run_process R --vanilla \
    -e "args<-commandArgs(trailingOnly=T);options(repos=args[1]);install.packages(args[-1])" \
    -q \
    --args 'https://cran.ism.ac.jp' "${packages[@]}"
}
