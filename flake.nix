{
  description = "A flake to tinker with godot and haskell.";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs;
  };

  outputs = { self, nixpkgs }:
  let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    hs = pkgs.haskellPackages;
    godotHaskellTest = hs.callCabal2nix "godotHaskellTest" ./. {};
  in
  {
    packages.x86_64-linux = {};

    devShells.x86_64-linux.default = hs.shellFor {
      packages = p: [ godotHaskellTest ];
      nativeBuildInputs = [
        hs.cabal-install
        hs.haskell-language-server
        pkgs.godot
      ];
    };
  };
}
