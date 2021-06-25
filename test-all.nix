# Nix configuration for testing phatsort against all supported GHC versions
#
# Usage:
#
#     $ nix-build test-all.nix

{
  phatsort-ghc-822 = import ./default.nix { compiler = "ghc822"; };
  phatsort-ghc-844 = import ./default.nix { compiler = "ghc844"; };
  phatsort-ghc-865 = import ./default.nix { compiler = "ghc865"; };
  phatsort-ghc-884 = import ./default.nix { compiler = "ghc884"; };
  phatsort-ghc-8104 = import ./default.nix { compiler = "ghc8104"; };
  phatsort-ghc-901 = import ./default.nix { compiler = "ghc901"; };
}
