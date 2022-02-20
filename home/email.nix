{ config, ... }:

let
  inherit (config.meta) email name;
in
{
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;
    hooks = {
      preNew = "mbsync --all";
    };
  };
  accounts.email = {
    accounts.mailbox = {
      realName = name;
      address = email;
      userName = email;
      primary = true;

      imap.host = "imap.mailbox.org";
      smtp.host = "smtp.mailbox.org";
      passwordCommand = "rbw get mailbox.org";

      notmuch.enable = true;
      msmtp.enable = true;
      mbsync = {
        enable = true;
        create = "maildir";
      };

      signature = {
        text = ''
          ${name}
          ---
          https://freire.dev.br
        '';
        showSignature = "append";
      };
    };
  };
}
