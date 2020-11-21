{ fetchFromGitHub, lib, pkgs, python3Packages, ... }:

with lib; rec {
  atcoder-tools = python3Packages.buildPythonApplication rec {
    pname = "atcoder-tools";
    version = "1.1.7.1";

    src = python3Packages.fetchPypi {
      inherit pname version;
      sha256 =
        "cfcee879e87b18f259c488e00553657d577f63dfea1afbf8b01b5401f4346238";
    };

    propagatedBuildInputs = with python3Packages; [
      beautifulsoup4
      colorama
      jinja2
      requests
      toml
    ];

    doCheck = false;

    meta = {
      license = licenses.mit;
      description =
        "Convenient modules & tools for AtCoder users, written in Python 3.5";
    };
  };

  colorlog = python3Packages.buildPythonPackage rec {
    pname = "colorlog";
    version = "4.6.2";

    src = python3Packages.fetchPypi {
      inherit pname version;
      sha256 =
        "54e5f153419c22afc283c130c4201db19a3dbd83221a0f4657d5ee66234a2ea4";
    };

    doCheck = false;

    meta = {
      license = licenses.mit;
      description = "A colored formatter for the python logging module";
    };
  };

  importlab = python3Packages.buildPythonPackage rec {
    pname = "importlab";
    version = "0.5.1";

    src = python3Packages.fetchPypi {
      inherit pname version;
      sha256 =
        "d855350d19dc10a17aabd2fe6f4b428ff1a936071f692fbf686a73694d26a51c";
    };

    propagatedBuildInputs = with python3Packages; [ networkx six ];

    doCheck = false;

    meta = {
      license = licenses.asl20;
      description =
        "A library that automatically infers dependencies for Python files";
    };
  };

  online-judge-api-client = python3Packages.buildPythonPackage {
    pname = "online-judge-api-client";
    version = "10.5.0";

    src = fetchFromGitHub {
      owner = "online-judge-tools";
      repo = "api-client";
      rev = "34b7c215af788252b69607487561f15f3634a33c";
      sha256 = "066g49kr2npb58d3ysc92qm6402248wk5sjhr3j31sk2v1pdjqn3";
    };

    propagatedBuildInputs = with python3Packages; [
      appdirs
      beautifulsoup4
      colorlog
      jsonschema
      lxml
      requests
      toml
    ];

    doCheck = false;

    meta = {
      license = licenses.mit;
      description = "API client to develop tools for competitive programming";
    };
  };

  online-judge-tools = python3Packages.buildPythonApplication {
    pname = "online-judge-tools";
    version = "11.1.1";

    src = fetchFromGitHub {
      owner = "online-judge-tools";
      repo = "oj";
      rev = "203c2f74c7f061fad57453a198f008cd5a41552e";
      sha256 = "0c5pyrz39ff0avhblpkxpk10w7hl2a4ladr524vrxvqhkhyhr3y7";
    };

    propagatedBuildInputs = with python3Packages; [
      colorama
      diff-match-patch
      online-judge-api-client
      requests
    ];

    doCheck = false;

    meta = {
      license = licenses.mit;
      description =
        "Tools for various online judges. Downloading sample cases, generating additional test cases, testing your code, and submitting it.";
    };
  };

  online-judge-verify-helper = python3Packages.buildPythonApplication {
    pname = "online-judge-verify-helper";
    version = "5.3.0";

    src = fetchFromGitHub {
      owner = "online-judge-tools";
      repo = "verification-helper";
      rev = "cee2c43359bf16470fe7981cab5517edceccfb6e";
      sha256 = "0ap01jrrr7h7xsfa7xrqs9j8k0nyxy0ghhkkl97vqfj45j2fa3cg";
    };

    propagatedBuildInputs = with python3Packages; [
      colorlog
      importlab
      online-judge-tools
      pyyaml
      requests
      setuptools
      toml
    ];

    doCheck = false;

    meta = {
      license = licenses.mit;
      description =
        "a testing framework for snippet libraries used in competitive programming";
    };
  };

  protonvpn-cli = python3Packages.buildPythonApplication rec {
    pname = "protonvpn-cli";
    version = "2.2.4";

    src = python3Packages.fetchPypi {
      inherit version;
      pname = "protonvpn_cli";
      sha256 =
        "e018976f38a666b9f7318b3c63bbefb5cf97db94e7963ef6e4abd3c3acd54b89";
    };

    propagatedBuildInputs = with python3Packages; [
      docopt
      jinja2
      pythondialog
      requests
    ];

    doCheck = false;

    meta = {
      license = licenses.gpl3;
      description =
        "Linux command-line client for ProtonVPN. Written in Python.";
    };
  };
}
