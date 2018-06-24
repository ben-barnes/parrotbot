{ mkDerivation, aeson, attoparsec, base, bytestring, hedgehog
, http-client, http-client-tls, http-types, stdenv, text, wai, warp
}:
mkDerivation {
  pname = "parrotbot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring http-client http-types text wai
  ];
  executableHaskellDepends = [
    base http-client http-client-tls warp
  ];
  testHaskellDepends = [ base hedgehog text ];
  license = stdenv.lib.licenses.publicDomain;
}
