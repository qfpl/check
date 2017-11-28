{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv =
    pkgs.haskell.lib.overrideCabal
      (import ./default.nix { inherit compiler; })
      (drv: {
        buildDepends = (drv.buildDepends or []) ++
          [ (haskellPackages.hoogleLocal {
              packages =
	        [ "check" ] ++
                drv.libraryHaskellDepends or [] ++
                drv.executableHaskellDepends or [];
              })
          ];
      });
in
  if pkgs.lib.inNixShell then drv.env else drv
