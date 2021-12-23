{ buildPythonApplication, colorlog, fetchFromGitHub, git, importlab, lib
, online-judge-tools, pyyaml, setuptools, substituteAll, toml, ... }:

buildPythonApplication rec {
  pname = "online-judge-verify-helper";
  version = "5.6.0";

  src = fetchFromGitHub {
    owner = "online-judge-tools";
    repo = "verification-helper";
    rev = "v${version}";
    sha256 = "0vv98ns81s0yibagi66g8mr5av1njpnirz0hdp8r1gpwnzz7s55h";
  };

  propagatedBuildInputs =
    [ colorlog importlab online-judge-tools pyyaml setuptools toml ];

  # Needs internet to run tests
  doCheck = false;

  meta = with lib; {
    description =
      "a testing framework for snippet libraries used in competitive programming";
    homepage = "https://github.com/online-judge-tools/verification-helper";
    license = licenses.mit;
    maintainers = with maintainers; [ sei40kr ];
  };
}
