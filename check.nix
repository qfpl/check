{ mkDerivation, aeson, base, containers, profunctors, stdenv, text
}:
mkDerivation {
  pname = "check";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base containers profunctors text ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/lightandlight/haskell-arrow-validation#readme";
  description = "Combinators for Arrow based data validation";
  license = stdenv.lib.licenses.bsd3;
}
