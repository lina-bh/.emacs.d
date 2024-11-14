{
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "emacs-overlay/nixpkgs";
  };

  outputs =
    {
      self,
      emacs-overlay,
      flake-utils,
      nixpkgs,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages.emacs-with-packages = emacs-overlay.lib.${system}.emacsWithPackagesFromUsePackage {
          config = ./init.el;
          package = emacs-overlay.packages.${system}.emacs-git;
          extraEmacsPackages =
            epkgs: with epkgs; [
              embark-consult
              caddyfile-mode
              markdown-mode
              nix-mode
              org
              (treesit-grammars.with-grammars (
                gram: with gram; [
                  tree-sitter-bash
                  tree-sitter-dockerfile
                  tree-sitter-java
                  tree-sitter-json
                  tree-sitter-tsx
                  tree-sitter-typescript
                ]
              ))
            ];
        };

        packages.emacs = pkgs.stdenvNoCC.mkDerivation (
          let
            emacs-with-packages = self.packages.${system}.emacs-with-packages;
          in
          let
            name = "emacs";
          in
          {
            inherit name;
            pname = name;
            dontUnpack = true;
            nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
            buildPhase = ''
              mkdir $out
              ln -s ${emacs-with-packages}/share $out/share
              for bin in emacs emacsclient; do
                makeBinaryWrapper ${emacs-with-packages}/bin/$bin $out/bin/$bin \
                  --inherit-argv0 \
                  --suffix PATH : ${
                    nixpkgs.lib.makeBinPath (
                      with pkgs;
                      [
                        nixfmt-rfc-style
                        ripgrep
                        yamlfmt
                      ]
                    )
                  }
              done
            '';
          }
        );

      }
    );
}
