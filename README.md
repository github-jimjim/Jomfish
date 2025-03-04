# Jomfish
# Jomfish Chess Engine

Jomfish is a powerful and feature-complete chess engine written in Rust, offering a highly competitive chess-playing experience. It is fully compliant with the Universal Chess Interface (UCI) protocol, making it easy to integrate with popular chess GUIs such as ChessBase, Arena, and others.

---

## Table of Contents

- [Key Features](#key-features)
- [Status](#Status)
- [Compile Process](#compile-process)
- [Apology & Clarification](#apology-&-clarification)
- [Licensing and Availability](#licensing-and-availability)
- [Why Choose Jomfish?](#why-choose-jomfish)

---

## Key Features

- **Stockfish 11 Parity**: Jomfish includes all major features found in Stockfish 11, providing a high-performance engine capable of competing at advanced levels of play.
- **No Neural Networks (NNUE)**: Unlike modern engines that rely on neural network-based evaluation, Jomfish exclusively uses classical evaluation methods. This ensures predictable and interpretable behavior during gameplay. But still it has a very strong performance with an estimated 3200 Elo
- **Rust-Based Development**: The engine was developed using the Rust programming language, focusing on performance, safety, and reliability.
- **Heavily based on Stockfish 11**: Though the engine was heavily inspired by the Stockfish 11 engine it still can't perform on his strenght. You can expect the same performance to Stockfish 9.

---

## Status

Jomfish is **no longer under active development**. The engine is considered feature-complete, as it includes all functionality comparable to Stockfish 11. No future updates or enhancements are planned.

---

## Compile Process

Before compiling the project, you need to rename the folder:

- **Step 1:** Rename the folder `src - compact` to `src`.
- **Step 2:** Change the directory to the Root directory and type:
```bash
cargo build --release
```

This step is crucial because the build scripts expect the source files to be in a folder named `src`.

---

## Apology & Clarification

I want to sincerely apologize for any complications caused by the initial release of Jomfish. A few months ago, when I first started using GitHub, I didn’t realize that releasing a chess engine required including a proper license and the full source code.

Additionally, I’d like to clarify that Jomfish is not a 1:1 port of Stockfish. While it is heavily inspired by it, I had to optimize it specifically for Rust, focusing on memory safety and performance improvements. Although a port might sound simple, it was far from easy due to the major differences between Rust and C++. 

---

## Licensing and Availability

Jomfish is available for use and modification under the Stockfish GNU General Public License v3.0. The engine is provided as-is, with no guarantees or support. 

[LICENSE](https://github.com/github-jimjim/Jomfish/blob/main/Copying.txt)

---

## Why Choose Jomfish?

Jomfish is ideal for users looking for a robust, NNUE-free chess engine with proven features and a strong competitive edge. While it does not adopt neural network advancements, it remains a reliable option for classical evaluation enthusiasts and UCI-compatible setups.

<img src="./logo.ico" alt="Logo" width="512" height="512">
