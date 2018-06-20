{ mkDerivation, aeson, attoparsec, base, bytestring, hedgehog
, http-types, slack-api, stdenv, text, wai, warp
}:
mkDerivation {
  pname = "parrotbot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring http-types text wai warp
  ];
  executableHaskellDepends = [ attoparsec base slack-api text ];
  testHaskellDepends = [ base hedgehog text ];
  license = stdenv.lib.licenses.publicDomain;
}
