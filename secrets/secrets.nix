# agenix recipients. Each entry maps a secret file to the public keys allowed
# to decrypt it. Re-run `agenix -r` after changing this file to rekey secrets.
let
  # People — keys allowed to edit & rekey every secret.
  users = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHcM1qHIUVmhRC0jY8Tzvu6SdTn+68cM7ArPw3AwD/LN"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIImly9yO1lUBeqsAgWYDHOYj8hYUg/zyvGb5X/qRsMNB"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOpr5uuTSdASh31etYaiBjqK9n6CBp8+ogG1V2b7ig2T"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJRpLEveihAeuMTA2PUZ3voPbK4KQVd0Hb4OZWD909cO"
  ];

  systems = {
    catarina = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGb/8HR7QfaLCB3HC78WkYI3H9lt0EflGnDSc2Y48aWx";
    joan = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBCnZ7jXHIzyVZ6nllROjNLtVUP/mSis9CBs3RvWx1m8";
  };

  allSystems = builtins.attrValues systems;
in
{
  "catarina/forgejo/secret-key.age".publicKeys = users ++ [ systems.catarina ];
  "catarina/forgejo/internal-token.age".publicKeys = users ++ [ systems.catarina ];
  "catarina/forgejo/admin-password.age".publicKeys = users ++ [ systems.catarina ];
  "catarina/forgejo/mailer-password.age".publicKeys = users ++ [ systems.catarina ];

  "catarina/grafana/admin-password.age".publicKeys = users ++ [ systems.catarina ];
  "catarina/grafana/secret-key.age".publicKeys = users ++ [ systems.catarina ];

  "catarina/restic/password.age".publicKeys = users ++ [ systems.catarina ];
  "catarina/restic/env.age".publicKeys = users ++ [ systems.catarina ];

  "joan/forgejo/runner-token.age".publicKeys = users ++ [ systems.joan ];
}
