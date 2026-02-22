# My Emacs Config

This is a modular Emacs configuration, expected to live in `~/.emacs.d/`. Split across multiple files for clarity and better organization.


## Prerequisites

Make sure the following are set up **before using** this config:

- `Hack` font is installed and available to your system. 
- `emacs-lsp-booster` is installed and available in your `$PATH`. My dotfiles repository has a helper script.
  - Install from: https://github.com/blahgeek/emacs-lsp-booster
- `~/.authinfo.gpg` exists with your credentials (for services like IRC).
- `~/.erc-credentials.el` exists for auto-login to IRC (see below).
- On macOS, you must have `exec-path-from-shell` properly pulling in relevant env vars like `LSP_USE_PLISTS`.
- Magit setup: https://magit.vc/manual/forge.html#Initial-Setup
- Golang setup:
  - go install golang.org/x/tools/gopls@latest
  - Ensure that $PATH has $GOPATH set in it.
  - debug configuration: https://emacs-lsp.github.io/dap-mode/page/configuration/#go
- C++ setup: (More info at: https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide)
  - Install clangd (https://clangd.llvm.org/installation.html)
  - On unix https://github.com/rizsotto/Bear or windows https://github.com/nickdiego/compiledb to generate compile_commands.json so that clangd can understand
  - In project root run:
    - On unix: `bear -- make -j$(nproc)`
    - On windows: `compiledb -n make` 
  - To enable clang formatting, create a clang format file in the project root, eg:
    - `clang-format -style=google -dump-config > .clang-format`
- Python setup:
  - Pyright must be installed, can install globally with:
    - `npm install -g pyright`
- AI Agent setup:
  - agent-shell: Follow pre-requisite setup instructions at: https://github.com/xenodium/agent-shell

## To activate
- Make a backup of your existing .emacs / .emacs.d if you haven't got it under source control.
- Delete any existing config
```sh
rm -rf ~/.emacs.d
```
- Create a symlink to these dotfiles. Replace `YOUR_CODE_PATH` with the location where these dotfiles exist.
```sh
ln -s ~/YOUR_CODE_PATH/dotfiles/emacs/.emacs.d ~/.emacs.d
```

## Optional Files
You should create the following if needed for IRC:

### `~/.erc-credentials.el`

With contents like:
```elisp
(setq erc-nick "your_nick")
```

