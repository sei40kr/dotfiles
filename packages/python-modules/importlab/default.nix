{ buildPythonPackage, fetchFromGitHub, lib, networkx, python27 }:

buildPythonPackage {
  pname = "importlab";
  version = "0.6.1";

  src = fetchFromGitHub {
    owner = "google";
    repo = "importlab";
    rev = "d9c968b2238b52e1a28b488c133675087fab041f";
    sha256 = "13y3i225y0hji65m2ricxw2q5pppra9i9w8aaqrbiz1mkp1zfrdl";
  };

  propagatedBuildInputs = [ networkx ];
  checkInputs = [ python27 ];

  meta = with lib; {
    description =
      "A library that automatically infers dependencies for Python files";
    homepage = "https://github.com/google/importlab";
    license = licenses.mit;
    maintainers = with maintainers; [ sei40kr ];
  };
}
