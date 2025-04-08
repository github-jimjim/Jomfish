# Jomfish

Jomfish is a powerful and competitive chess engine written in Rust. It is fully compliant with the Universal Chess Interface (UCI) protocol, ensuring seamless integration with popular chess GUIs like ChessBase, Arena, and more.

---

## Table of Contents

- [Key Features](#key-features)
- [Status](#status)
- [Compile Process](#compile-process)
- [Apology & Clarification](#apology--clarification)
- [Licensing and Availability](#licensing-and-availability)
- [Why Choose Jomfish?](#why-choose-jomfish)

---

## Key Features

- **Hybrid NNUE Evaluation**: Jomfish integrates neural networks in a hybrid fashion. Unlike Stockfish, which relies fully on NNUE, Jomfish blends classical evaluation with NNUE techniques to explore a more balanced approach.
- **Rust-Based Core**: Developed entirely in Rust, the engine leverages the language's strengths in memory safety, concurrency, and performance.
- **Inspired by Stockfish 11**: While the classical evaluation draws from Stockfish 11, the NNUE component is uniquely implemented.

---

## Status

Jomfish has resumed active development and is now better than ever with integrated NNUE evaluation. This marks the beginning of a new line of releases: the H series. The latest release is **Jomfish H1 Beta**.

---

## Compile Process

To compile Jomfish:

1. Rename the folder as needed.
2. Open a terminal in the project root directory.
3. Run the following command:

```bash
cargo build --release
```

---

## Apology & Clarification

I sincerely apologize for any confusion caused by the initial release of Jomfish. As a new GitHub user at the time, I wasn’t fully aware of the importance of proper licensing and full source code inclusion for engine releases.

To clarify: Jomfish is not a direct 1:1 port of Stockfish. While it is heavily inspired by it, adapting the engine to Rust required deep structural changes, particularly in memory management and performance optimization.

---

## Licensing and Availability

Jomfish is released under the GNU General Public License v3.0, in line with Stockfish’s licensing terms. It is open-source and available for use, modification, and distribution.

[View LICENSE](https://github.com/github-jimjim/Jomfish/blob/main/Copying.txt)

---

## Why Choose Jomfish?

Jomfish is an excellent choice for users seeking a strong, innovative chess engine that blends classical evaluation with neural networks. Whether you're an enthusiast or a developer, Jomfish provides a competitive and customizable experience, fully UCI-compatible and designed with performance and safety in mind.

<img src="./logo.ico" alt="Logo" width="512" height="512">

