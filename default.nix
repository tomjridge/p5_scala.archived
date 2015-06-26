{ }: 
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    sbt = pkgs.sbt;
in stdenv.mkDerivation {
      name = "p5_scala";
    
      src=./.;
    
      buildInputs = [ sbt ];
    
      buildPhase="sbt compile";
 
      installPhase="mkdir -p $out && cp -R * $out"; # so we can inspect the result
    
}
