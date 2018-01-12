{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, exceptions, fetchgit
, lifted-async, mmorph, monad-control, mtl, pretty-show, primitive
, random, resourcet, semigroups, stdenv, stm, template-haskell
, text, th-lift, time, transformers, transformers-base, unix
, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "0.5.1";
  src = fetchgit {
    url = "https://github.com/LightAndLight/haskell-hedgehog";
    sha256 = "1ran4yb3lj63zn18g2x5s8ym000h7z36pjhsxl5r3d8kfzvs2qw7";
    rev = "593be732ee8bcb69c9b89244bd19a8c6b9e18c2b";
  };
  postUnpack = "sourceRoot+=/hedgehog/; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory exceptions lifted-async mmorph monad-control mtl
    pretty-show primitive random resourcet semigroups stm
    template-haskell text th-lift time transformers transformers-base
    unix wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers pretty-show text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Hedgehog will eat all your bugs";
  license = stdenv.lib.licenses.bsd3;
}
