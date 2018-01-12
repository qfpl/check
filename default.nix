{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  hedgehog = import ./nix/hedgehog.nix;

  overrides = 
    self: super:
    {
      hedgehog = self.callPackage hedgehog {};
    } //
      (if compiler == "ghc7103" || compiler == "ghc7102"
      then {
        transformers = super.transformers_0_5_4_0;
        }
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

  nixpkgs = import <nixpkgs> { inherit config; };

  f = import ./check.nix;

  haskellPackages =
    if compiler == "default"
      then nixpkgs.haskellPackages
      else nixpkgs.haskell.packages.${compiler};
in
  haskellPackages.callPackage f {}

