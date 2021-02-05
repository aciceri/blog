{
  description = "My blog";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    flake-utils.url = github:numtide/flake-utils;
    nur.url = github:nix-community/NUR;
    skeleton-src = {
      url = github:atomicpages/skeleton-sass;
      flake = false;
    };
    katex-src = {
      url = "https://github.com/KaTeX/KaTeX/releases/download/v0.12.0/katex.tar.gz";
      flake = false;
    };
    baskerville-src = {
      url = "https://github.com/impallari/Libre-Baskerville/archive/master.zip";
      flake = false;
    };
    firacode-src = {
      url = github:tonsky/FiraCode;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, nur, skeleton-src, katex-src, baskerville-src, firacode-src }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ nur.overlay ]; };
        blog = pkgs.callPackage ./blog.nix {
          inherit pkgs;
          thirdparty = [
            {
              name = "skeleton";
              path = "${skeleton-src}/src";
            }
            {
              name = "katex";
              path = "${katex-src}";
            }
            {
              name = "baskerville";
              path = "${baskerville-src}";
            }
            {
              name = "firacode";
              path = "${firacode-src}/distr/woff2";
            }
          ];
        };
      in
        rec {
          packages = { inherit (blog) generator ci shell; };
          defaultPackage = blog.generator;
          apps.compile =
            flake-utils.lib.mkApp { drv = blog.ci.compile; exePath = ""; };
          apps.generator =
            flake-utils.lib.mkApp { drv = blog.generator-with-thirdparty; exePath = "/bin/generator"; };

          defaultApp = apps.compile;
          devShell = blog.shell;
        }
    );
}
  
