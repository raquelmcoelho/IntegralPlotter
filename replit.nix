{ pkgs }: {
    deps = [
        pkgs.gfortran
        pkgs.gnuplot
        # This ls available on nix is for XL fortran 
        # pkgs.fortran-language-server
    ];
}