let
  users = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIEpBZXIOn5Eeq2peV4gH3jSf2fqinRnTPHd1NHlscLZ"
  ];
in
{
  "mailbox.age".publicKeys = users;
}
