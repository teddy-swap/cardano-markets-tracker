########################################################################
# default.nix -- The top-level nix build file for cardano-markets-tracker.
#
# This file defines various attributes that are used for building and
# developing cardano-markets-tracker.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   cardano-markets-tracker: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix;

  inherit (packages) pkgs cardano-markets-tracker;
  project = cardano-markets-tracker.haskell.project;
in
{
  inherit pkgs cardano-markets-tracker;

  inherit project;
}