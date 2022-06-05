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

    # Usage: pick file-pattern [starting-point...]
    #
    # Finds all files matching `file-pattern`, starting from each `starting-point`,
    # or from the current directory if none are provided. If more than one exists,
    # prompts the user to select one. Prints to stdout the selected one path
    # that was found or selected.
    pick = pkgs.writeShellScriptBin "pick" ''
      readarray -t paths < <(find ''${@:2} -name ''${1:?Missing file-pattern} -type f -print)

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

    # Usage: upfind starting-point [arg...]
    #
    # Finds files starting at `starting-point` and moving upwards through
    # the directory hierarchy until reaching /.  Prints their paths to
    # stdout in the order they're found.  Any `arg`s provided are passed on
    # to `find`.
    upfind = pkgs.writeShellScriptBin "upfind" ''
      cd ''${1:?Missing starting-point}
      shift 1
      while [[ $PWD != / ]]; do
        find "$PWD"/ -maxdepth 1 "$@"
        cd ..
      done
    '';

    # Usage: gd [-s search-string] [-e] [starting-point...]
    #
    # Without any options provided, runs the godot project found by searching within
    # each `starting-point`, or the current directory if none are provided. If more
    # than one such project is found, the user is prompted to select one.
    #
    # If the `-s` flag is provided, along with a required `search-string`, instead of
    # running the godot project, runs a scene (.tscn) file within the project. The
    # `search-string` is expected to be a substring of the desired scene file name,
    # and is used to filter down the list of scenes. If one scene remains after
    # filtering, that is what is run.  If more than one remains, the user is prompted
    # to select one.
    #
    # If the `-e` flag is provided, the project or scene is opened for editing instead
    # of being run.
    #
    # Note: Before invoking godot, this searches upwards for the nearest cabal file
    # and builds it. Assuming the godot project has a symbolic link to the cabal build
    # output, this ensures that godot has the latest build of the code.
    gd = pkgs.writeShellScriptBin "gd" ''
      TYPE=project
      GODOTFLAG=
      SCENESUBSTR=""

      while getopts sn:e FLAG; do
        case $FLAG in
          s)
            TYPE=scene
            SCENESUBSTR="$OPTARG"
          ;;
          e) GODOTFLAG=-e ;;
          \?) >&2 echo "Invalid option: -$OPTARG"; return 1 ;;
        esac
      done
      shift $((OPTIND-1))

      PROJECT="$(pick project.godot $@)"

      if [ -z "$PROJECT" ]; then
        >&2 echo "No godot project found!"
        exit 1
      fi
      
      PROJECTDIR="$(dirname $PROJECT)"

      CABALFILE="$(upfind $PROJECTDIR -name ?*.cabal -type f -print | head -n 1)"

      if [ -z "$CABALFILE" ]; then
        >&2 echo "No cabal file found!"
        exit 1
      fi

      cabal build "$CABALFILE"

      case $TYPE in
        project) godot $GODOTFLAG --path "$PROJECTDIR" & ;;
        scene)
          SCENEPATH="$(pick "*$SCENESUBSTR*.tscn" "$PROJECTDIR")"

          if [ -z "$SCENEPATH" ]; then
            >&2 echo "No godot scene found!"
            exit 1
          fi

          godot $GODOTFLAG --path "$PROJECTDIR" "$(basename "$SCENEPATH")" &
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

        pick
        gd
        upfind
      ];
    };
  };
}
