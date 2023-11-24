# Support
1. If you have any problem, please report
   [a GitHub issue to the movebit/sui-move-analyzer-issue repository](https://github.com/movebit/sui-move-analyzer-issue) to get help.
2. Welcome to the developer discussion group as well：
   https://t.me/moveanalyzer

# Sui-Move-Analyzer
We are providing comprehensive language support for the Move programming language. Currently, this means that the basic syntax and language configurations of Move (.Move) are enabled, allowing you to enjoy features such as syntax highlighting, simple context-independent completion suggestions, and other essential language functions within your files. This enhances your efficiency and overall experience when coding and manipulating files.

## How to Install
The `sui-move-analyzer` Visual Studio Code extension works via two components: the `sui-move-analyzer language server` and the extension itself.
You need to install SuiCli refer as https://docs.sui.io/references/cli before install `sui-move-analyzer`.

### 1. Installing the `sui-move-analyzer language server`<span id="Step1">
`sui-move-analyzer language server` may be installed in one of two ways:

#### A. Download the precompiled installer for the `sui-move-analyzer language server`.(Recommended)

```Windows```  Download [sui-move-analyzer-win-installer-v1.1.1.msi](https://github.com/movebit/move/releases), and proceed with the installation. This installation program will automatically add the path of 'sui move analyzer' to the environment variable.

```MacOS```
 Download the binary program [sui-move-analyzer-mac-v1.1.1](https://github.com/movebit/move/releases) and add its path to the environment variable by yourself.

```Linux```
 Download the binary program [sui-move-analyzer-ubuntu-v1.1.1](https://github.com/movebit/move/releases) and add its path to the environment variable by yourself.

#### B. Use Cargo
   Use Rust's package manager `cargo` to install `sui-move-analyzer` in your user's PATH. This
   is recommended for people who do not work on core Move.
   1. If you don't already have a Rust toolchain installed, you should install
      [Rustup](https://rustup.rs/), which will install the latest stable Rust toolchain.

   2. Invoke `cargo install --git https://github.com/movebit/move --branch sui_move_analyzer sui-move-analyzer` to install the
      `sui-move-analyzer` language server in your Cargo binary directory. On macOS and Linux, this is
      usually `~/.cargo/bin`. You'll want to make sure this location is in your `PATH` environment
      variable. If you plan to use the language server with Move language flavors different from core Move,
      you should specify an additional option to `cargo install` command as different Move flavors
      may enforce different max length of the Move address type: `--features "address20"` option for Move
      flavors requiring 20-byte long addresses (e.g., Sui Move) and `--features "address32"` option
      for Move flavors requiring 32-byte long addresses (e.g., Aptos Move).

To confirm that you've installed the language server program successfully, execute
`sui-move-analyzer --version` on the command line. You should see the output `sui-move-analyzer version number(v1.1.1)`.
If you don't see it, check the troubleshooting section at the end.

After installation, restart VSCode.

### 2. Installing the sui-move-analyzer Visual Studio Code extension

1. Open a new window in any Visual Studio Code application version 1.55.2 or greater.
2. Open the command palette (`⇧⌘P` on macOS, or use the menu item *View > Command Palette...*) and
   type **Extensions: Install Extensions**. This will open a panel named *Extensions* in the
   sidebar of your Visual Studio Code window.
3. In the search bar labeled *Search Extensions in Marketplace*, type **sui-move-analyzer**. The
   sui-move-analyzer extension should appear in the list below the search bar. Click **Install**.
4. Open any file that ends in `.move`. Or to create a new file, click **Select a language**, and
   choose the **Move** language. As you type, you should see that keywords and types appear in
   different colors.

### Troubleshooting
Please note: If you don't see the version number, you can refer to the troubleshooting section."

#### [1] cannot find the 'sui-move-analyzer' program
##### 1) windows
If you are installing this extension on a Windows system and have followed the steps in Section 1.A by running the windows-installer.msi, but executing 'sui-move-analyzer --version' in the command line doesn't find the 'sui-move-analyzer' program, the issue may be that VSCode cannot locate the configured environment variables. You can try the following:

   1. Restart VSCode and install the 'sui-move-analyzer' VSCode extension.
   2. In the Windows system settings, find the user environment variable 'Path.' Look for an entry ending with 'MoveBit\sui-move-analyzer\,' and copy it.
   3. Open the extension settings for 'sui-move-analyzer' in the VSCode extension store. In the 'sui-move-analyzer > server:path' entry, add the path ending with 'MoveBit\sui-move-analyzer\' before 'sui-move-analyzer.' The final result should look like: 'C:\Users\Windows\AppData\Local\Apps\MoveBit\sui-move-analyzer\'
   4. Try running 'sui-move-analyzer --version' in the command line again.

##### 2) mac & linux
If you see an error message *language server executable 'sui-move-analyzer' could not be found* in the
bottom-right of your Visual Studio Code screen when opening a Move file, it means that the
`sui-move-analyzer` executable could not be found in your `PATH`. You may try the following:

1. Confirm that invoking `sui-move-analyzer --version` in a command line terminal prints out
   `sui-move-analyzer version number`. If it doesn't, then retry the instructions in [step 1](./Step1). If it
   does successfully print this output, try closing and re-opening the Visual Studio Code
   application, as it may not have picked up the update to your `PATH`.
2. If you installed the `sui-move-analyzer` executable to a different location that is outside of your
   `PATH`, then you may have the extension look at this location by using the the Visual Studio Code
   settings (`⌘,` on macOS, or use the menu item *Code > Preferences > Settings*). Search for the
   `sui-move-analyzer.server.path` setting, and set it to the location of the `sui-move-analyzer` language
   server you installed.
3. If you're using it in MacOS, you may meet the error `Macos cannot verify if this app contains malicious software`, you need to add support for `sui-move-analyzer-mac-v1.1.1` in the system settings Program Trust.


#### [2] analyzer not work
Open a Move source file (a file with a .move file extension) and if the opened Move source file is located within a buildable project (a Move.toml file can be found in one of its parent directories), the following advanced features will be available:

  - compiler diagnostics
  - go to definition
  - go to references
  - type on hover
  - autocomplete
  - ...

Therefore, the Move.toml file must be found in the project directory for the plug-in's functionality to take effect.

In addition, if you have already opened the move project before, the installed plug-in will not take effect in time. You need to reopen the vscode window and open the move project code again before the plug-in is activated. 

#### [3] build failed with steps in Section 1.B
If `cargo install --git https://github.com/movebit/move --branch sui_move_analyzer sui-move-analyzer` run failed, and meet the 
error info as follows:
```
error: failed to run custom build command for librocksdb-sys...

--- stderr
thread 'main' panicked at 'Unable to find libclang: "couldn't find any valid shared libraries matching: 
['clang.dll', 'libclang.dll']..."'
```

It's because it relies on `MystenLabs/sui_move_build` library, which requires an LLVM environment. You can refer to [llvm-project](https://github.com/llvm/llvm-project) go and install llvm.


## Features

Here are some of the features of the sui-move-analyzer Visual Studio Code extension. To see them, open a
Move source file (a file with a `.move` file extension) and:

- See Move keywords and types highlighted in appropriate colors.
- As you type, Move keywords will appear as completion suggestions.
- If the opened Move source file is located within a buildable project (a `Move.toml` file can be
  found in one of its parent directories), the following advanced features will also be available:
  - compiler diagnostics
  - sui commands line tool(you need install Sui Client CLI locally)
  - sui project template
  - go to definition
  - go to references
  - type on hover
  - inlay hints
  - linter for move file
  - ...
