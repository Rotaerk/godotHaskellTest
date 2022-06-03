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
#    hs = pkgs.haskell.packages.ghc902;
#    hs = pkgs.haskellPackages;

    editgame = pkgs.writeShellScriptBin "editgame" ''
      readarray -t projects < <(find -name project.godot -type f -print)

      case "''${#projects[@]}" in
        0) echo "No godot project found!" ;;
        1) godot -e "''${projects[0]}" & ;;
        *)
          select project in "''${projects[@]}"; do
            godot -e "''${project}" &
            break
          done
        ;;
      esac
    '';
  in
  {
    packages.x86_64-linux = { inherit pkgs; };

    devShells.x86_64-linux.default = hs.shellFor {
      packages = p: [];
      nativeBuildInputs = [
        hs.cabal-install
        hs.haskell-language-server
        pkgs.godot
        editgame
      ];
      extraDependencies = p: {
        libraryHaskellDepends = [ p.shake ];
      };
    };
  };
}
