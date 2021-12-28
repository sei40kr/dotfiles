{ fetchFromGitHub, lib, vimUtils, ... }:

with lib;
let
in {
  impatient-nvim = vimUtils.buildVimPluginFrom2Nix {
    pname = "impatient.nvim";
    version = "2021-12-26";
    src = fetchFromGitHub {
      owner = "lewis6991";
      repo = "impatient.nvim";
      rev = "3ea9abedb6941995b05fdad654d9cfd51c38a31f";
      sha256 = "06b8h3g77wrjxvhapkvx149pha29a0zcq28bj2pcvh7686cysz9k";
    };
    meta.homepage = "https://github.com/lewis6991/impatient.nvim/";
  };

  lua-dev-nvim = vimUtils.buildVimPluginFrom2Nix {
    pname = "lua-dev.nvim";
    version = "2021-12-31";
    src = fetchFromGitHub {
      owner = "folke";
      repo = "lua-dev.nvim";
      rev = "03a44ec6a54b0a025a633978e8541584a02e46d9";
      sha256 = "1id96h1kl299mlgqqby4kcnsb6nz51r5i4vyfdcnpla27w0pr6pd";
    };
    meta.homepage = "https://github.com/folke/lua-dev.nvim/";
  };

  nvim-lspconfig = vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-lspconfig";
    version = "2021-12-31";
    src = fetchFromGitHub {
      owner = "neovim";
      repo = "nvim-lspconfig";
      rev = "4b21740aae18ecec2d527b79d1072b3b01bb5a2a";
      sha256 = "1qzzll8m9ry685vx31sp88ix5gkaky4a18awdyz25cdxm83dmv83";
    };
    meta.homepage = "https://github.com/neovim/nvim-lspconfig/";
  };

  nvim-ts-autotag = vimUtils.buildVimPluginFrom2Nix {
    pname = "nvim-ts-autotag";
    version = "2021-12-19";
    src = fetchFromGitHub {
      owner = "windwp";
      repo = "nvim-ts-autotag";
      rev = "0ceb4ef342bf1fdbb082ad4fa1fcfd0f864e1cba";
      sha256 = "0bbjhjngn0wv6f28z437bx9743w366665ygz6pz81059whfp93g7";
    };
    meta.homepage = "https://github.com/windwp/nvim-ts-autotag/";
  };

  surround-nvim = vimUtils.buildVimPluginFrom2Nix {
    pname = "surround.nvim";
    version = "2021-12-29";
    src = fetchFromGitHub {
      owner = "blackCauldron7";
      repo = "surround.nvim";
      rev = "81f997bd71590f21f717e24bae72edf8e8b7b0f6";
      sha256 = "06mdbpka2z17hyi3p7b9ksyz79ppwybhr4h9829qba8xr2gpd465";
    };
    meta.homepage = "https://github.com/blackCauldron7/surround.nvim/";
  };
}
