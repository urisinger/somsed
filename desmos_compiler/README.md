# Desmos Compiler

`desmos_compiler` is a Rust-based project that aims to improve the performance of Desmos graphs by compiling expressions into native assembly.

## Installation

### Prerequisites

- Rust (latest stable version)
- LLVM development libraries (`llvm-sys`)
- Cargo (Rust package manager)

### Instructions

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/desmos_compiler.git
   cd desmos_compiler
   ```

2. Install required dependencies:
   ```bash
   # Install LLVM on your system. For example:
   sudo apt install llvm-dev  # Ubuntu
   sudo pacman -S llvm        # Arch Linux 
   brew install llvm          # macOS
   ```

3. Build the project:
   ```bash
   cargo build --release
   ```

4. Run the project:
   ```bash
   cargo run --release
   ```

## Usage

For now the `desmos_compiler` executable accepts Desmos-like expressions from standard input, and compiles them into llvm bytescode. In the future it would either become just a library or a full blown gui application for graphing desmos expressions.

## Contributing

Contributions are welcome! If you have any ideas for improving the performance or adding new features, feel free to open an issue or submit a pull request.

### Steps to Contribute

1. Fork the repository.
2. Create a new branch for your feature or bug fix:
   ```bash
   git checkout -b feature/my-feature```
3. Commit your changes:
   ```bash
   git commit -m "Add my new feature"```
4. Push to your branch:
   ```bash
   git push origin feature/my-feature```
5. Open a pull request.

you can also just ask me for push accses.
