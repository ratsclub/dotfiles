{ super, ... }:

{
  programs.chromium = {
    enable = true;
    extensions = [
      # ublock origin
      "cjpalhdlnbpafiamejdnhcphjbkeiagm"

      # bitwarden
      "nngceckbapebfimnlniiiahkandclblb"

      # privacy badger
      "pkehgijcmpdhfbdbbnkijodmdjhbjlgp"
    ];
  };
}
