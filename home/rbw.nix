{config, ...}:

let
  inherit (config.meta) email;
  in
{
  programs.rbw = {
    enable = true;
    settings = {
      inherit email;
      pinentry = "gnome3";
    };
  };
}
