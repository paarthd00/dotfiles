# dotfiles

Personal Linux dotfiles managed with symlinks.

## What this repo manages

- Shell: `.zshrc`, `.zprofile`, `.zshenv`, `.p10k.zsh`
- Neovim: `.config/nvim/` (Lua config with `lazy.nvim`)
- Alacritty: `.config/alacritty/`
- tmux local overrides: `.config/tmux/tmux.conf.local`
- XMonad config: `.xmonad/xmonad.hs`

## Quick start

```sh
git clone git@github.com:paarthd00/dotfiles.git ~/dotfiles
cd ~/dotfiles
./bootstrap.sh
```

`bootstrap.sh` creates symlinks in `$HOME`. If a target already exists, it is moved to a timestamped backup like `*.bak-YYYYMMDD-HHMMSS`.

## Prerequisites

Minimum tools:

- `zsh`
- `git`
- `neovim` (0.9+ recommended)
- `tmux`
- `alacritty`

Useful extras used by this config:

- `ripgrep`, `fd` or `fdfind`, `fzf`, `bat`, `tree`
- `make` (for Telescope FZF native build)
- `xclip` (used by `fzfc` helper in `.zshrc`)
- `prettier`, `stylua` (used by `none-ls` formatting)

## Neovim notes

- Plugin manager: `lazy.nvim` (auto-installs on first run).
- LSP management: `mason.nvim` + `mason-lspconfig.nvim`.
- Configured servers include: `clangd`, `phpactor`, `gopls`, `pyright`, `rust_analyzer`, `ts_ls`, `html`, `lua_ls`.
- Treesitter parsers are installed from config for common languages (Go, Lua, Python, PHP, Rust, TS/JS, Bash, Haskell, etc.).
- Copilot is enabled via `zbirenbaum/copilot.lua` and requires `:Copilot auth`.

## Shell notes

- Uses Oh My Zsh + Powerlevel10k.
- Enabled plugins: `git`, `zsh-autosuggestions`, `fast-syntax-highlighting`, plus local `zsh-autocomplete`.
- The config includes many machine-specific PATH entries under `$HOME` and `/opt`.

## Privacy and secrets

- `~/.gitconfig` is intentionally **not** managed by `bootstrap.sh`.
- This repository should not store personal emails, tokens, private keys, or machine-specific credentials.
- `.gitconfig` is ignored via `.gitignore`.

Set Git identity locally on each machine:

```sh
git config --global user.name "Your Name"
git config --global user.email "your-email@example.com"
```

## Repository layout

```text
.
├── bootstrap.sh
├── .zshrc
├── .zprofile
├── .zshenv
├── .p10k.zsh
├── .config/
│   ├── nvim/
│   ├── alacritty/
│   └── tmux/
└── .xmonad/
```
