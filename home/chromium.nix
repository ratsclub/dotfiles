{ super, ... }:

{
  programs.chromium = {
    enable = super.device.type == "graphical";
    extensions = [
      # ublock origin
      "cjpalhdlnbpafiamejdnhcphjbkeiagm"

      # bitwarden
      "nngceckbapebfimnlniiiahkandclblb"
    ];
  };
}
