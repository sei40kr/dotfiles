{ fetchFromGitHub, lib, vimUtils, ... }:

with lib;
{
  mini-nvim = vimUtils.buildVimPluginFrom2Nix {
    pname = "mini.nvim";
    version = "2022-01-06";
    src = fetchFromGitHub {
      owner = "echasnovski";
      repo = "mini.nvim";
      rev = "a1aa674e94c81feb1fc210527324bfb1e4a08b6f";
      sha256 = "1blpk5p1lpd87ramnp5nqv188p8wdxsg8d1w811pmxqwas2ji7f5";
    };
    meta.homepage = "https://github.com/echasnovski/mini.nvim";
  };

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
