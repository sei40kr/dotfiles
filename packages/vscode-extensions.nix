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

  castwide.solargraph = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "solargraph";
      publisher = "castwide";
      version = "0.21.1";
      sha256 = "15dy6pg4cm0marj5wf5swqk60vmm0xbz284r26jhyn2jai8ccb33";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  ccls-project.ccls = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "ccls";
      publisher = "ccls-project";
      version = "0.1.29";
      sha256 = "1q0cs5fnj42a5jc0zgdfsypk63zil6y3gam67rhfk2qvp021hcs6";
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

  esbenp.prettier-vscode = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "prettier-vscode";
      publisher = "esbenp";
      version = "5.1.3";
      sha256 = "03i66vxvlyb3msg7b8jy9x7fpxyph0kcgr9gpwrzbqj5s7vc32sr";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  felixfbecker.php-intellisense =
    pkgs.vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "php-intellisense";
        publisher = "felixfbecker";
        version = "2.3.14";
        sha256 = "19jw0yh7gir8mr9hpglg5gcdhag1wdbh0z9mfww81dbj27gab61p";
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

  golang.Go = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "Go";
      publisher = "golang";
      version = "0.15.1";
      sha256 = "1h7r781asl890n9fc0dh81l4ffx8xqd81d4hy2680dji8x390axz";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  rust-lang.rust = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "rust";
      publisher = "rust-lang";
      version = "0.7.8";
      sha256 = "039ns854v1k4jb9xqknrjkj8lf62nfcpfn0716ancmjc4f0xlzb3";
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

  redhat.vscode-xml = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "vscode-xml";
      publisher = "redhat";
      version = "0.13.0";
      sha256 = "013h12qhj1h1pcjns2l5dn2hb52p8j468f5n5sw62gq4vfr9yvyf";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  redhat.vscode-yaml = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "vscode-yaml";
      publisher = "redhat";
      version = "0.8.0";
      sha256 = "08dy5wm24c3bga698925pjwbymdmxi00a84d6xajj750pax7grz0";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  REditorSupport.r-lsp = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "r-lsp";
      publisher = "REditorSupport";
      version = "0.1.6";
      sha256 = "005ff28d5b6ih7hxfjb7qd1qj4x3x2ndrv82bzj4naf421rxaz4b";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  Pivotal.vscode-boot-dev-pack =
    pkgs.vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "vscode-boot-dev-pack";
        publisher = "Pivotal";
        version = "0.0.8";
        sha256 = "1128xwmy6yf7w1l5pg1kdgrfkldxgvk2bbdlw7h4wk4l5l7mi44h";
      };
      meta.license = stdenv.lib.licenses.mit;
    };

  TabNine.tabnine-vscode = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "tabnine-vscode";
      publisher = "TabNine";
      version = "2.8.8";
      sha256 = "07n35nnq0qcdb8nv25zrzsvjypcyq8qk9mwbz0cnh59dvvz5lgp0";
    };
    meta.license = stdenv.lib.licenses.mit;
  };

  vscjava.vscode-java-pack = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "vscode-java-pack";
      publisher = "vscjava";
      version = "0.9.1";
      sha256 = "14b65bpgy8r2qm6fr06ph81qpv69yxdxpgx1lbg6xfhf0rp9mafw";
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
