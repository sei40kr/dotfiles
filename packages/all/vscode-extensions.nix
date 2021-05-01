{ lib, pkgs, stdenv, ... }:

with lib; {
  alefragnani.Bookmarks = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "Bookmarks";
      publisher = "alefragnani";
      version = "11.3.1";
      sha256 = "0g4a18r6qqclp5q02nxf5pjyrv6y9l9xrmqblgbjlmh25bzvwgz0";
    };
    meta.license = licenses.mit;
  };

  castwide.solargraph = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "solargraph";
      publisher = "castwide";
      version = "0.21.1";
      sha256 = "15dy6pg4cm0marj5wf5swqk60vmm0xbz284r26jhyn2jai8ccb33";
    };
    meta.license = licenses.mit;
  };

  ccls-project.ccls = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "ccls";
      publisher = "ccls-project";
      version = "0.1.29";
      sha256 = "1q0cs5fnj42a5jc0zgdfsypk63zil6y3gam67rhfk2qvp021hcs6";
    };
    meta.license = licenses.mit;
  };

  eamodio.gitlens = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "gitlens";
      publisher = "eamodio";
      version = "10.2.2";
      sha256 = "00fp6pz9jqcr6j6zwr2wpvqazh1ssa48jnk1282gnj5k560vh8mb";
    };
    meta.license = licenses.mit;
  };

  felixfbecker.php-intellisense =
    pkgs.vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "php-intellisense";
        publisher = "felixfbecker";
        version = "2.3.14";
        sha256 = "19jw0yh7gir8mr9hpglg5gcdhag1wdbh0z9mfww81dbj27gab61p";
      };
      meta.license = licenses.mit;
    };

  rust-lang.rust = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "rust";
      publisher = "rust-lang";
      version = "0.7.8";
      sha256 = "039ns854v1k4jb9xqknrjkj8lf62nfcpfn0716ancmjc4f0xlzb3";
    };
    meta.license = licenses.mit;
  };

  redhat.vscode-xml = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "vscode-xml";
      publisher = "redhat";
      version = "0.13.0";
      sha256 = "013h12qhj1h1pcjns2l5dn2hb52p8j468f5n5sw62gq4vfr9yvyf";
    };
    meta.license = licenses.mit;
  };

  REditorSupport.r-lsp = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "r-lsp";
      publisher = "REditorSupport";
      version = "0.1.6";
      sha256 = "005ff28d5b6ih7hxfjb7qd1qj4x3x2ndrv82bzj4naf421rxaz4b";
    };
    meta.license = licenses.mit;
  };

  Pivotal.vscode-boot-dev-pack =
    pkgs.vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "vscode-boot-dev-pack";
        publisher = "Pivotal";
        version = "0.0.8";
        sha256 = "1128xwmy6yf7w1l5pg1kdgrfkldxgvk2bbdlw7h4wk4l5l7mi44h";
      };
      meta.license = licenses.mit;
    };

  vscjava.vscode-java-pack = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "vscode-java-pack";
      publisher = "vscjava";
      version = "0.9.1";
      sha256 = "14b65bpgy8r2qm6fr06ph81qpv69yxdxpgx1lbg6xfhf0rp9mafw";
    };
    meta.license = licenses.mit;
  };

  vscodevim.vim = pkgs.vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "vim";
      publisher = "vscodevim";
      version = "1.14.5";
      sha256 = "013h12qhj1h1pcjns2l5dn2hb52p8j468f5n5sw62gq4vfr9yvyf";
    };
    meta.license = licenses.mit;
  };
}
