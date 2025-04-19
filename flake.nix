{
  description = "Root flake for my machines";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable-small";
  };

  outputs = {nixpkgs, ...}: let
    pkgs = import nixpkgs {system = "x86_64-linux";};
  in {
    formatter.x86_64-linux = pkgs.alejandra;
    devShells.x86_64-linux.default = pkgs.mkShell {
      shellHook = ''
        cargo install pest-language-server
        
        export PATH="$PATH:$HOME/.cargo/bin"
      '';
      packages = with pkgs; [
        libffi
        libxml2
        lldb
        llvm
        nil
        valgrind
      ];
    };
  };
}
