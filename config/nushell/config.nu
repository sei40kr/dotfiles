$env.config = {
  show_banner: false
}

source @nu_scripts@/modules/data_extraction/ultimate_extractor.nu
alias x = extract

source @nu_scripts@/nu-hooks/nu-hooks/direnv/direnv.nu

source @atuin_nu@
source @zoxide_nu@
