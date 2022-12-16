{
  description = "My blog";

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://aciceri-blog.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "aciceri-blog.cachix.org-1:RsBUsFem/Lr0ItecDFfhmXvxvs3WRqVlCQlxNqRXgWw="
    ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    haskellNix.url = "github:input-output-hk/haskell.nix";

    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    katex = {
      url = "https://github.com/KaTeX/KaTeX/releases/download/v0.16.0/katex.tar.gz";
      flake = false;
    };

    latin-modern = {
      url = "github:aaaakshat/cm-web-fonts";
      flake = false;
    };

    fira-code = {
      url = "https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip";
      type = "file";
      flake = false;
    };

    hyphenopoly = {
      url = "https://github.com/mnater/Hyphenopoly/archive/refs/tags/v4.12.0.zip";
      flake = false;
    };

    asciinema = {
      url = "https://registry.npmjs.org/asciinema-player/-/asciinema-player-3.0.1.tgz";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
    ...
  } @ inputs:
    flake-utils.lib.eachDefaultSystem (
      system: let
        overlays = [
          haskellNix.overlay
          (final: _: {
            blogGeneratorProject = final.haskell-nix.project' {
              src = ./generator;
              compiler-nix-name = "ghc924";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
              };
            };
          })
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.blogGeneratorProject.flake {};

        thirdpartyFarm = with inputs; let
          fira-code-unzipped = pkgs.runCommand "extract-fira-code" {} ''
            mkdir $out
            cd $out
            ${pkgs.unzip}/bin/unzip ${fira-code};
          '';
        in
          pkgs.linkFarm "thirdparty" [
            {
              name = "katex";
              path = katex;
            }
            {
              name = "latin-modern";
              path = "${latin-modern}/font";
            }
            {
              name = "fira-code";
              path = "${fira-code-unzipped}/woff2";
            }
            {
              name = "hyphenopoly";
              path = hyphenopoly;
            }
            {
              name = "asciinema";
              path = "${asciinema}/dist/bundle";
            }
          ];

        generator = flake.packages."blog:exe:generator".overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs or [] ++ [pkgs.makeWrapper];
          installPhase =
            old.installPhase
            + "\n"
            + ''
              wrapProgram $out/bin/generator --set THIRDPARTY ${thirdpartyFarm}
            '';
        });

        blog = pkgs.stdenv.mkDerivation {
          name = "blog";
          src = self; # TODO: use `builtins.filterSource`
          buildInputs = [pkgs.git];
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
        nixpkgs.lib.recursiveUpdate flake {
          packages = {
            inherit generator thirdpartyFarm blog;
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
