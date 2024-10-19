{
  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.follows = "emacs-overlay/flake-utils";
    nixpkgs.follows = "emacs-overlay/nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      emacs-overlay,
      flake-utils,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        emacsPackagesFor = emacs-overlay.lib.${system}.emacsPackagesFor;
        emacs-pgtk = emacs-overlay.packages.${system}.emacs-pgtk;
      in
      {
        packages = rec {
          emacs-with-packages = (
            (emacsPackagesFor emacs-pgtk).emacsWithPackages (
              epkgs: with epkgs; [
                cape
                consult
                corfu
                elfeed
                embark
                embark-consult
                envrc
                magit
                marginalia
                nix-mode
                orderless
                org
                puni
                prettier
                reformatter
                rust-mode
                treesit-auto
                vterm
                yaml-mode
                aggressive-indent
                (treesit-grammars.with-grammars (
                  gram: with gram; [
                    tree-sitter-bash
                    tree-sitter-dockerfile
                    tree-sitter-java
                    tree-sitter-json
                    tree-sitter-lua
                    tree-sitter-rust
                    tree-sitter-tsx
                    tree-sitter-typescript
                  ]
                ))
              ]
            )
          );

          emacs-with-tools = pkgs.buildEnv {
            name = "emacs-with-tools";
            paths = with pkgs; [
              emacs-with-packages
              ripgrep
              nixfmt-rfc-style
              nodePackages.prettier
              gitMinimal
              taplo
            ];
          };
        };
      }
    );
}
