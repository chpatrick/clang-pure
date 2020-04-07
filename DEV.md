# Development guide <small>[&#8617;][readme]</small>

## Install prerequisites

You'll need a recent version of [LLVM/Clang][llvm]. clang-pure has been tested
with at least LLVM version 9, but newer versions should work.

### Debian/Ubuntu

Follow [these instructions](https://apt.llvm.org/) to install LLVM version 9 (or later).

You may need to set the following environment variables:
```bash
export CLANG_PURE_LLVM_LIB_DIR=/usr/lib/llvm-9/lib;
export CLANG_PURE_LLVM_INCLUDE_DIR=/usr/lib/llvm-9/include;
```
### Windows

Install the LLVM 9 (or newer) [package](https://releases.llvm.org/download.html) and install to the default
location, which is typically `%PROGRAMFILES%\LLVM`.

### Mac OS X

Build LLVM version 9 (or newer) from source or install using
[Homebrew][brew] (easiest):

```bash
$ brew install llvm@9
```

## Building

### Nix

Run `nix-shell` in the root directory to get a development shell or `nix-build` to build a Nix package.

### Stack

Build using [Stack][stack] from the repository root:

```bash
$ stack setup
$ stack build
```

If the setup script is unable to detect your LLVM/libclang library paths
automatically, you can override the default search location using the
`CLANG_PURE_LLVM_INCLUDE_DIR` and `CLANG_PURE_LLVM_LIB_DIR` variables and then re-run the build.

On Linux or Mac OS X:

```bash
$ CLANG_PURE_LLVM_INCLUDE_DIR=/path/to/llvm/include CLANG_PURE_LLVM_LIB_DIR=/path/to/llvm/lib stack build
```

On Windows:

```cmd
> set CLANG_PURE_LLVM_INCLUDE_DIR=C:\Path\To\LLVM\include
> set CLANG_PURE_LLVM_LIB_DIR=C:\Path\To\LLVM\bin
> stack build
```

The default search location for libraries and include directories is `/usr`
under Linux and Mac OS X and `%PROGRAMFILES%\LLVM` on Windows.

[brew]: http://brew.sh/
[llvm]: http://llvm.org/
[llvm381]: http://llvm.org/releases/download.html#3.8.1
[readme]: README.md
[stack]: https://haskellstack.org/
