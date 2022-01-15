{ fetchFromGitHub, lib, vimUtils, ... }:

with lib;
{
  octo-nvim = vimUtils.buildVimPluginFrom2Nix {
    pname = "octo.nvim";
    version = "2022-01-04";
    src = fetchFromGitHub {
      owner = "pwntester";
      repo = "octo.nvim";
      rev = "1cb910a7a42398580618ac4f3c3731214ea0916d";
      sha256 = "1fa3shk9h296fnjv47a315d6b9cf4kjlaxa37nibd1hp2l0l8v5d";
    };
    meta.homepage = "https://github.com/pwntester/octo.nvim";
  };
}
