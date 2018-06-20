{ mkDerivation, attoparsec, base, slack-api, stdenv, text }:
mkDerivation {
  pname = "parrotbot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base slack-api text ];
  license = stdenv.lib.licenses.bsd3;
}
