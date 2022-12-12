{
  description = "Simple flake with flake-utils";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/a704b9029586266f63807f64a6718f1a65b0f83b";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        hello = pkgs.hello;
        helloApp = flake-utils.lib.mkApp { drv = hello; };

      in {
        packages = { hello = hello; };
        defaultPackage = hello;

        apps = { hello = helloApp; };
        defaultApp = helloApp;

        devShell = pkgs.mkShell {
          LD_LIBRARY_PATH = (pkgs.zlib.outPath + "/lib");
          inputsFrom = [ pkgs.hello ];
          buildInputs = with pkgs; [ zlib ];
          nativeBuildInputs = [ ];
        };
      });
}
