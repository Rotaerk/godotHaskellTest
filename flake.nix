{
  description = "A flake to tinker with godot and haskell.";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
  };

  outputs = { self, nixpkgs }:
  let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
#    hs = pkgs.haskell.packages.ghc8107;
    hs = pkgs.haskell.packages.ghc922;
#    hs = pkgs.haskellPackages;
  in
  {
    packages.x86_64-linux = { inherit pkgs; };

    devShells.x86_64-linux.default = hs.shellFor {
      packages = p: [];
      nativeBuildInputs = [
        hs.cabal-install
        hs.haskell-language-server
        hs.hpack
        pkgs.godot
        pkgs.zlib
      ];
    };
  };
}
