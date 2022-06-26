{
  description = "My blog";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    haskellNix.url = github:input-output-hk/haskell.nix;
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            blogGeneratorProject =
              final.haskell-nix.project' {
                src = ./generator;
                compiler-nix-name = "ghc8107";
                shell.tools = {
                  cabal = { };
                  haskell-language-server = { };
                };
              };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.blogGeneratorProject.flake { };

        thirdparty = with pkgs; [
          {
            name = "katex";
            path = fetchTarball {
              url = "https://github.com/KaTeX/KaTeX/releases/download/v0.16.0/katex.tar.gz";
              sha256 = "0qk199nxq9w7k1vzf6rqjbaddyibw6mh2178yq8axv6gxdhbycfc";
            };
          }
          {
            name = "latin-modern";
            path = ''${fetchFromGitHub {
              owner = "aaaakshat";
              repo = "cm-web-fonts";
              rev = "fa103f6a8b06101de5819cf5274d18a65e9a947e";
              sha256 = "1rskjfcxvahaf5xp0zkji9limybfl5slp8fsgkh9a4lmadcrq941";
            }}/font'';
          }
          {
            name = "firacode";
            path = ''${fetchFromGitHub {
              owner = "tonsky";
              repo = "FiraCode";
              rev = "e2e526c9716a7f5d00f26627f1bb0949aaef2d2a";
              sha256 = "2KHY6y/9AzTNdHQmqzG5dllH5/jpOCDJkU6IQ8grHEk=";
            }}/distr/woff2'';
          }
          {
            name = "hyphenopoly";
            path = fetchTarball {
              url = "https://github.com/mnater/Hyphenopoly/archive/refs/tags/v4.12.0.zip";
              sha256 = "0p13dykajx3cymmp8fl79mr79ch9v7mbjq8hn41alpqyl44sw2iw";
            };
          }
          {
            name = "asciinema";
            path = ''${fetchTarball {
              url = "https://registry.npmjs.org/asciinema-player/-/asciinema-player-3.0.1.tgz";
              sha256 = "1qlsrl7wkxz5yy4j9x5jlz6civ97zlk2fydcs2ny45h934qh7sc1";
            }}/dist/bundle'';
          }
        ];

        thirdpartyFarm = pkgs.linkFarm "thirdparty" thirdparty;

        generator = flake.packages."blog:exe:generator".overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs or [ ] ++ [ pkgs.makeWrapper ];
          installPhase = old.installPhase + "\n" + ''
            wrapProgram $out/bin/generator --set THIRDPARTY ${thirdpartyFarm}
          '';
        });

        blog = pkgs.stdenv.mkDerivation {
          name = "blog";
          src = ./.; # TODO: use `builtins.filterSource`
          buildInputs = [ pkgs.git ];
          # The `sed` hack is necessary until we got https://github.com/jaspervdj/hakyll/issues/700
          buildPhase = ''
            sed -i '/^#+title.*/i ----' posts/*.org
            sed -i '/^#+language.*/a ----' posts/*.org
            sed -ri 's/\#\+(title|tags|date|language)\:/\1:/g' posts/*.org

            mkdir -p generator/thirdparty/
            ln -sfn ${thirdpartyFarm}/* generator/thirdparty/

            ${generator}/bin/generator build
          '';
          installPhase = ''
            mkdir $out
            cp -r out/* $out
          '';
          dontFixup = true;
        };

      in
      flake // {
        packages = {
          inherit generator blog;
          default = blog;
        };
        apps = {
          default = flake-utils.lib.mkApp {
            drv = generator;
            exePath = "/bin/generator";
          };
        };
      }
    );
}
  
