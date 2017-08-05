{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let hsPkgs = nixpkgs.pkgs.haskell.packages.${compiler};
    unliftio-core = hsPkgs.callPackage ./deps/unliftio-core.nix { };
    unliftio = hsPkgs.callPackage ./deps/unliftio.nix { inherit unliftio-core ; };
in hsPkgs.callPackage ./unliftio-concurrent.nix { inherit unliftio-core; inherit unliftio; }
