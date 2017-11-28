{ compiler ? "default" }:

let
  _nixpkgs = import <nixpkgs> {};
  unstable = _nixpkgs.fetchFromGitHub {
    owner = "NixOS"; 
    repo = "nixpkgs";
    rev = "7a87f165ebb0ff7a662654b2c5a3820feedd54ab";
    sha256 = "1qywbg9fm3jay9jhwc7qr8yj9pzn3n63f01axd6g2dvgdalr39f1";
  };

  hedgehog = import ./nix/hedgehog.nix;

  overrides = 
	  self: super:
	    {
	      attoparsec = super.attoparsec_0_13_2_0;
	      hedgehog = self.callPackage hedgehog {};
	      these = super.these_0_7_4;
            } //
	    (if compiler == "ghc7103" || compiler == "ghc7102"
	      then { transformers = super.transformers_0_5_5_0; }
	      else {});

  config = {
    packageOverrides = pkgs:
      if compiler == "default"
        then {
          haskellPackages = pkgs.haskellPackages.override {
            inherit overrides;
          };
        }
        else {
          haskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
              ${compiler} = pkgs.haskell.packages.${compiler}.override { inherit overrides; };
            };
          };
        };
  };

  nixpkgs = import unstable { inherit config; };

  f = import ./check.nix;

  haskellPackages =
    if compiler == "default"
      then nixpkgs.haskellPackages
      else nixpkgs.haskell.packages.${compiler};
in
  haskellPackages.callPackage f {}

