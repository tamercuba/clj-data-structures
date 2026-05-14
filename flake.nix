{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      devShells.x86_64-linux.default = pkgs.mkShell {
        name = "clj-data-structures";
        packages = [
          pkgs.pandoc
          pkgs.texlive.combined.scheme-medium
        ];
        shellHook = ''
          export NIX_SHELL_NAME="clj-data-structures"
        '';
      };
    };
}
