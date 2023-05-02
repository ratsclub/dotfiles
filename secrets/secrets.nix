let
  victor = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE8w7K7WeGfbdcTOM2lfXhEWKI+pNgFzNOwM8HkTIABz"
  ];
in
{
  "mailbox.age".publicKeys = victor;
}
