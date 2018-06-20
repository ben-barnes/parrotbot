{ pkgs ? import <nixpkgs> { }}:
{
  parrotbot = pkgs.haskellPackages.callPackage ./parrotbot.nix { };
}
