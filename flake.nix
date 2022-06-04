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

    pickgd = pkgs.writeShellScriptBin "pickgd" ''
      PATTERN=project.godot
      while getopts s: FLAG; do
        case $FLAG in
          s) PATTERN="*$OPTARG*.tscn" ;;
          \?) >&2 echo "Invalid option: -$OPTARG"; exit 1 ;;
        esac
      done
      shift $((OPTIND-1))

      readarray -t paths < <(find $1 -name $PATTERN -type f -print)

      case "''${#paths[@]}" in
        0) exit ;;
        1) echo "''${paths[0]}" ;;
        *)
          select path in "''${paths[@]}"; do
            echo "''${path}"
            break
          done
        ;;
      esac
    '';

    gd = pkgs.writeShellScriptBin "gd" ''
      TYPE=project
      GODOTFLAG=
      SCENESUBSTR=""

      while getopts sn:e FLAG; do
        case $FLAG in
          s) TYPE=scene ;;
          n)
            TYPE=scene
            SCENESUBSTR="$OPTARG"
          ;;
          e) GODOTFLAG=-e ;;
          \?) >&2 echo "Invalid option: -$OPTARG"; return 1 ;;
        esac
      done
      shift $((OPTIND-1))

      PROJECTDIR="$(dirname $(pickgd $1))"

      case $TYPE in
        project) godot $GODOTFLAG --path "$PROJECTDIR" ;;
        scene) godot $GODOTFLAG --path "$PROJECTDIR" "$(basename $(pickgd -s "$SCENESUBSTR" "$PROJECTDIR"))" ;;
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

        pickgd
        gd
      ];

      extraDependencies = p: {
        libraryHaskellDepends = [ p.shake ];
      };
    };
  };
}
