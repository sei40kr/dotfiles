{ fetchFromGitHub, git, lib, vimUtils, ... }:

with lib;
let
in {
  dein-vim = vimUtils.buildVimPluginFrom2Nix rec {
    pname = "dein-vim";
    version = "2.2";

    src = fetchFromGitHub {
      owner = "Shougo";
      repo = "dein.vim";
      rev = version;
      sha256 = "0p4w9qb714vz11c9mk2hs3zkirmyb8c6zinci9qb4d9v0hvnrgzz";
    };

    dependencies = [ git ];
  };

  onedark-nvim = vimUtils.buildVimPluginFrom2Nix rec {
    pname = "onedark-nvim";
    version = "unstable-2021-12-01";

    src = fetchFromGitHub {
      owner = "navarasu";
      repo = "onedark.nvim";
      rev = "ce49cf36dc839564e95290e2cdace396c148bca1";
      sha256 = "1q7y7vhgk8r28nq4q3aamvkg2hfylfxq25i2avj45prap23b73zy";
    };
  };
}
