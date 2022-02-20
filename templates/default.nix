{
  # nix flake init -t github:ratsclub/dotfiles#templates.rust
  rust = {
    description = "Rust Project Template";
    path = ./rust;
  };

  # nix flake init -t github:ratsclub/dotfiles#templates.go
  go = {
    description = "Go Project Template";
    path = ./go;
  };

  # nix flake init -t github:ratsclub/dotfiles#templates.basic
  basic = {
    description = "A very basic flake";
    path = ./basic;
  };
}
