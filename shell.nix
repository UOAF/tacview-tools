let pkgs = (import <nixos> {});
in pkgs.mkShell {
    buildInputs = with pkgs; [ zlib ];
}
