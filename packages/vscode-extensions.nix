{ config, lib, pkgs, stdenv, ... }:

with lib; {
  alefragnani.Bookmarks = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "Bookmarks";
      publisher = "alefragnani";
      version = "11.3.1";
      sha256 = "0g4a18r6qqclp5q02nxf5pjyrv6y9l9xrmqblgbjlmh25bzvwgz0";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  CoenraadS.bracket-pair-colorizer-2 =
    pkgs.vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "bracket-pair-colorizer-2";
        publisher = "CoenraadS";
        version = "0.1.4";
        sha256 = "08sawrq6ib5dr7774i1hsiwxzg1lljdhysrsvazrbdw7ldbsaz30";
      };
      meta.license = stdenv.lib.licenses.mit;
    };

  dbaeumer.vscode-eslint = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "vscode-eslint";
      publisher = "dbaeumer";
      version = "2.1.5";
      sha256 = "1fr8n736pfjnxz7kgi6m86d2w6z1bwxa23xpfcd8sik9nnrnsz59";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  eamodio.gitlens = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "gitlens";
      publisher = "eamodio";
      version = "10.2.2";
      sha256 = "00fp6pz9jqcr6j6zwr2wpvqazh1ssa48jnk1282gnj5k560vh8mb";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  esbenp.prettier-vscode = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "prettier-vscode";
      publisher = "esbenp";
      version = "5.1.3";
      sha256 = "03i66vxvlyb3msg7b8jy9x7fpxyph0kcgr9gpwrzbqj5s7vc32sr";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  EditorConfig.EditorConfig =
    pkgs.vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "EditorConfig";
        publisher = "EditorConfig";
        version = "0.15.1";
        sha256 = "18r19dn1an81l2nw1h8iwh9x3sy71d4ab0s5fvng5y7dcg32zajd";
      };
      meta.license = stdenv.lib.licenses.mit;
    };

  GitHub.vscode-pull-request-github =
    pkgs.vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "vscode-pull-request-github";
        publisher = "GitHub";
        version = "0.17.0";
        sha256 = "1ga1fs3kihy70sgz2nfzdmq7162p7vav2g1p0kmbas5vszs12g0a";
      };
      meta.license = stdenv.lib.licenses.mit;
    };

  mechatroner.rainbow-csv = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "rainbow-csv";
      publisher = "mechatroner";
      version = "1.7.0";
      sha256 = "1i5avr2zf6n2xmfidfgxhzmhy5f85gkxva0vaj1k0fv0rznfcq5p";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  vscodevim.vim = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "vim";
      publisher = "vscodevim";
      version = "1.14.5";
      sha256 = "013h12qhj1h1pcjns2l5dn2hb52p8j468f5n5sw62gq4vfr9yvyf";
    };
    meta.license = stdenv.lib.licenses.mit;
  };
}
