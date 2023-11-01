
# aptos-move-analyzer


**Table of Contents**
* [Introduction](#Introduction)
* [Installation](#Installation)
* [Features](#Features)
* [Support](#Support)
* [Changelogs](#Changelogs)


## Introduction <span id="Introduction">
The **aptos-move-analyzer** is a Visual Studio Code plugin for **Aptos Move** language developed by [MoveBit](https://movebit.xyz). Although this is an alpha release, it has many useful features, such as **highlight, autocomplete, go to definition/references**, and so on.


## Installation <span id="Installation">

**Note**:

If you already have installed *move-analyzer* or *sui-move-analyzer*, please disable them before installing **aptos-move-analyzer**, because it may have some conflicts.

### How to Install (Must Read)

The `aptos-move-analyzer` Visual Studio Code extension works via two components: the `extension` itself and the `aptos-move-analyzer` language server. Below are two steps that describe how to install all of them.

### 1. Installing the `aptos-move-analyzer` language server<span id="Step1">

The `aptos-move-analyzer` language server is a Rust program, so we suggest installing it via `cargo`. If you haven't installed the Rust toolchain, you can install [Rustup](https://rustup.rs/), which will install the latest stable Rust toolchain including `cargo`.

**Execute the below command to install `aptos_move_analyzer`**
```
cargo install --git https://github.com/movebit/move --branch feature/aptos_move_analyzer aptos-move-analyzer
```

The installation may take some time, often several minutes. After installation, the `aptos-move-analyzer` program is in your `cargo` binary directory. On macOS and Linux, this directory is usually `~/.cargo/bin`. You should make sure this location is in your `PATH` environment variable via `export PATH="$PATH:~/.cargo/bin"` .

To confirm that you've installed the language server program successfully, execute `aptos-move-analyzer --version` on the command line. You should see the output `aptos-move-analyzer version number`.


### 2. Installing the aptos-move-analyzer Visual Studio Code extension

1. Open a new window in any Visual Studio Code application version 1.55.2 or greater.
2. Open the command palette (`⇧⌘P` on macOS, or use the menu item *View > Command Palette...*) and type **Extensions: Install Extensions**. This will open a panel named *Extensions* in the sidebar of your Visual Studio Code window.
3. In the search bar labeled *Search Extensions in Marketplace*, type **aptos-move-analyzer**. The aptos-move-analyzer extension should appear in the list below the search bar. Click **Install**.
4. Open any Aptos Move project directory(where the Move.toml is located), and open or create files that end in `.move`, you should see that keywords and types appear in different colors, and you can try other features.

### Troubleshooting

If you see an error message *language server executable `aptos-move-analyzer` could not be found* in the bottom-right of your Visual Studio Code screen when opening a Move file, it means that the `aptos-move-analyzer` executable could not be found in your `PATH`. You may try the following:

1. Confirm that invoking `aptos-move-analyzer --version` in a command line terminal prints out `aptos-move-analyzer version number`. If it doesn't, then retry the instructions in **[Step 1](#Step1)**. If it does successfully print this output, try closing and re-opening the Visual Studio Code application, as it may not have picked up the update to your `PATH`.
2. If you installed the `aptos-move-analyzer` executable to a different location that is outside of your `PATH`, then you may have the extension look at this location by using the the Visual Studio Code settings (`⌘,` on macOS, or use the menu item *Code > Preferences > Settings*). Search for the `aptos-move-analyzer.server.path` setting, and set it to the location of the `aptos-move-analyzer` language server you installed.
3. If the above steps don't work, then [report an issue](#Support) to get help.


## Features <span id="Features">

Here are some of the features of the aptos-move-analyzer Visual Studio Code extension. To see them, open a
Move source file (a file with a `.move` file extension) and:

- See Move keywords and types highlighted in appropriate colors.
- Comment and un-comment lines of code using the `⌘/` shortcut on macOS (or the menu command *Edit >
  Toggle Line Comment*).
- Place your cursor on a delimiter, such as `<`, `(`, or `{`, and its corresponding delimiter --
  `>`, `)`, or `}` -- will be highlighted.
- As you type, Move keywords will appear as completion suggestions.
- If the opened Move source file is located within a buildable project (a `Move.toml` file can be
  found in one of its parent directories), the following advanced features will also be available:
  - compiler diagnostics
  - go to definition
  - go to type definition
  - go to references
  - type on hover
  - outline view showing symbol tree for Move source files
  - autocomplete


## Support <span id="Support">

If you find any issues, please join the [MoveAnalyzer](https://t.me/moveanalyzer) Telegram developer discussion group and report issues.


## Changelogs <span id="Changelogs">

### 2023/07/25 0.0.2
* Added inlay hint to variable.
* Added inlay hint to parameter.
* Added inlay hint to statement.
* Auto reload move.toml when modified. 

### 2023/07/21 0.0.1
* Added semantic analysis to the Move language and Aptos Move, and enhanced some features of the plug-in, such as go-to-definition, auto-completion, finding references, etc.;
* Integrated common Aptos development commands into Command Palette, support Aptos Code Snippets automatic completion (currently only init function);
* Support parallel development of multiple projects under the same directory;

