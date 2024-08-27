{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devkitnix = {
      url = "github:knarkzel/devkitnix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, devkitnix}:
    let
      pkgs = import nixpkgs {system = "x86_64-linux";};
      devkitARM = devkitnix.packages.x86_64-linux.devkitARM;
    in {
      devShells.x86_64-linux = {
        default = pkgs.mkShell {
          buildInputs = [
            devkitARM
            pkgs.mgba
          ];
          shellHook = ''
            export DEVKITPRO=${devkitARM}
            export DEVKITARM=${devkitARM}/devkitARM
            export PATH=$DEVKITARM/bin:$PATH
          '';
        };
      };
    };
}
