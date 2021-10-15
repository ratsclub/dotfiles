{
  nixpkgs.overlays = [
    # GNOME fractional scaling support
    (self: super: {
      gnome = super.gnome.overrideScope' (gself: gsuper: {
        mutter = gsuper.mutter.overrideAttrs (oldAttrs: {
          patches = oldAttrs.patches ++ [
            # https://salsa.debian.org/gnome-team/mutter/-/blob/ubuntu/master/debian/patches/x11-Add-support-for-fractional-scaling-using-Randr.patch
            (super.fetchpatch {
              url = "https://salsa.debian.org/gnome-team/mutter/-/raw/91d9bdafd5d624fe1f40f4be48663014830eee78/debian/patches/x11-Add-support-for-fractional-scaling-using-Randr.patch";
              sha256 = "m6PKjVxhGVuzsMBVA82UyJ6Cb1s6SMI0eRooa+F2MY8=";
            })
          ];
        });

        gnome-control-center = gsuper.gnome-control-center.overrideAttrs (oldAttrs: {
          patches = oldAttrs.patches ++ [
            # https://salsa.debian.org/gnome-team/gnome-control-center/-/blob/ubuntu/master/debian/patches/ubuntu/display-Support-UI-scaled-logical-monitor-mode.patch
            (super.fetchpatch {
              url = "https://salsa.debian.org/gnome-team/gnome-control-center/-/raw/f185f33fb200cc963c062c7a82920a085f696978/debian/patches/ubuntu/display-Support-UI-scaled-logical-monitor-mode.patch";
              sha256 = "XBMD0chaV6GGg3R9/rQnsBejXspomVZz/a4Bvv/AHCA=";
            })
            # https://salsa.debian.org/gnome-team/gnome-control-center/-/blob/ubuntu/master/debian/patches/ubuntu/display-Allow-fractional-scaling-to-be-enabled.patch
            (super.fetchpatch {
              url = "https://salsa.debian.org/gnome-team/gnome-control-center/-/raw/f185f33fb200cc963c062c7a82920a085f696978/debian/patches/ubuntu/display-Allow-fractional-scaling-to-be-enabled.patch";
              sha256 = "Pm6PTmsL2bW9JAHD1u0oUEqD1PCIErOlcuqlwvP593I=";
            })
          ];
        });
      });
    })
  ];
}
