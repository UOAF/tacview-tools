let pkgs = (import <nixos> {});
in pkgs.haskell.lib.buildStackProject {
    name = "tacview";
    buildInputs = with pkgs; [ zlib ];
}
