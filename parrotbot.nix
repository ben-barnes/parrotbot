{ mkDerivation, attoparsec, base, hedgehog, slack-api, stdenv, text
}:
mkDerivation {
  pname = "parrotbot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ attoparsec base text ];
  executableHaskellDepends = [ attoparsec base slack-api text ];
  testHaskellDepends = [ hedgehog ];
  license = stdenv.lib.licenses.publicDomain;
}
