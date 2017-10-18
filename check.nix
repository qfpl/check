{ mkDerivation, aeson, base, containers, hedgehog, profunctors
, stdenv, text, these
}:
mkDerivation {
  pname = "check";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers profunctors text these
  ];
  testHaskellDepends = [ base containers hedgehog ];
  homepage = "https://github.com/lightandlight/haskell-arrow-validation#readme";
  description = "Combinators for Arrow based data validation";
  license = stdenv.lib.licenses.bsd3;
}
