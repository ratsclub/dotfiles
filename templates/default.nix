{
  # nix flake init -t github:ratsclub/dotfiles#templates.rust
  rust = {
    description = "Rust Project Template";
    path = ./rust;
  };

  # nix flake init -t github:ratsclub/dotfiles#templates.rust
  go = {
    description = "Go Project Template";
    path = ./go;
  };
}
