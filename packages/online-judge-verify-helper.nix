{ pkgs }:

pkgs.python3Packages.buildPythonApplication rec {
  pname = "online-judge-verify-helper";
  version = "5.6.0";
  pyproject = true;

  src = pkgs.fetchFromGitHub {
    owner = "online-judge-tools";
    repo = "verification-helper";
    rev = "v${version}";
    sha256 = "0vv98ns81s0yibagi66g8mr5av1njpnirz0hdp8r1gpwnzz7s55h";
  };

  build-system = [ pkgs.python3Packages.setuptools ];

  propagatedBuildInputs = with pkgs.python3Packages; [
    colorlog
    importlab
    online-judge-tools
    pyyaml
    toml
  ];

  # Needs internet to run tests
  doCheck = false;

  meta = with pkgs.lib; {
    description = "a testing framework for snippet libraries used in competitive programming";
    homepage = "https://github.com/online-judge-tools/verification-helper";
    license = licenses.mit;
    maintainers = with maintainers; [ sei40kr ];
  };
}
