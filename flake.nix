{
  description = "A flake to tinker with godot and haskell.";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs;
  };

  outputs = { self, nixpkgs }:
  let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    hs = pkgs.haskell.packages.ghc8107;
#    godotHaskellTest = hs.callCabal2nix "godotHaskellTest" ./. {};
  in
  {
    packages.x86_64-linux = { inherit pkgs; };

    devShells.x86_64-linux.default = hs.shellFor {
#      packages = p: [ godotHaskellTest ];
      packages = p: [];
      nativeBuildInputs = [
        hs.cabal-install
        hs.haskell-language-server
        pkgs.godot
      ];
    };
  };
}
