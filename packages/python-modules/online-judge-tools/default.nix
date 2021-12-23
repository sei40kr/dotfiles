{ buildPythonPackage, colorama, fetchFromGitHub, lib, online-judge-api-client
, requests, ... }:

buildPythonPackage rec {
  pname = "online-judge-tools";
  version = "11.5.1";

  src = fetchFromGitHub {
    owner = "online-judge-tools";
    repo = "oj";
    rev = "v${version}";
    sha256 = "0zkzmmjgjb6lyrzq1ip54cpnp7al9a7mcyjyi5vx58bvnx3q0c6m";
  };

  propagatedBuildInputs = [ colorama online-judge-api-client requests ];

  # Needs internet access
  doCheck = false;

  pythonImportsCheck = [ "onlinejudge_command" ];

  meta = with lib; {
    description =
      "Tools for various online judges. Downloading sample cases, generating additional test cases, testing your code, and submitting it.";
    homepage = "https://github.com/online-judge-tools/oj";
    license = licenses.mit;
    maintainers = with maintainers; [ sei40kr ];
  };
}
