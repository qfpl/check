{ mkDerivation, aeson, base, containers, hedgehog, mtl, profunctors
, semigroups, semigroupoids, stdenv, transformers, text, these
}:
mkDerivation {
  pname = "check";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers mtl profunctors semigroupoids text these semigroups
  ];
  testHaskellDepends = [
    base containers hedgehog mtl profunctors these transformers
  ];
  homepage = "https://github.com/lightandlight/haskell-arrow-validation#readme";
  description = "Combinators for Arrow based data validation";
  license = stdenv.lib.licenses.bsd3;
}
