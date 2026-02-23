# dotfiles

Personal Linux dotfiles managed with symlinks.

## Application overview

This repo ships a CLI-driven dotfiles setup flow with theme-aware config management.

- `setup-cli` (shell wrapper): runs the Haskell executable `setup/Main.hs` via cabal.
- `setup/Main.hs` (main app): detects installed tools, manages symlinks/backups, and applies a selected theme.
- `bootstrap.sh` (shell wrapper): runs `setup-cli --setup-only --yes` for first-time base linking.
- `theme-cli/Main.hs` and `bootstrap/Main.hs`: older standalone Haskell flows kept in-repo for reference.

What it configures:

- shell (`.zshrc`, `.zprofile`, `.zshenv`, `.p10k.zsh`)
- tmux (`~/.tmux.conf`)
- Alacritty (`~/.config/alacritty/alacritty.toml`)
- Neovim theme file (`~/.config/nvim/lua/paarth/theme.lua`)
- Codex CLI theme (`~/.codex/config.toml`)
- Claude CLI theme (`~/.claude.json`)
- optional XMonad config (if installed)

## Architecture and flow

This is a dotfiles repository delivered through a Haskell setup tool.

Execution flow:

1. `bootstrap.sh` -> `cabal run setup-cli -- --setup-only --yes`
2. `setup-cli` (wrapper script) -> `cabal run setup-cli -- ...`
3. Haskell app (`setup/Main.hs`) performs:
   - base dotfile linking (`runSetup`)
   - theme selection/apply (`runThemeFlow`)

Practical takeaway:

- You usually run `./setup-cli ...` (or `cabal run setup-cli -- ...`).
- `bootstrap.sh` is just a convenience shortcut for initial non-interactive setup-only runs.

## What setup manages

- Shell: `.zshrc`
- Neovim: `.config/nvim/` (Lua config with `lazy.nvim`)
- Alacritty: `.config/alacritty/`
- tmux: `~/.tmux.conf` 
- XMonad config: `.xmonad/xmonad.hs` (optional)

## Quick start

```sh
git clone git@github.com:paarthd00/dotfiles.git ~/dotfiles
cd ~/dotfiles
./bootstrap.sh
```

`bootstrap.sh` runs `setup-cli --setup-only --yes` and creates symlinks in `$HOME`. Existing targets are replaced in place.

## Installation

1. Install prerequisites:
   - `git`, `zsh`, `tmux`, `alacritty`, `neovim`
   - Haskell toolchain (`ghc`, `cabal`)
2. Clone the repository:

```sh
git clone git@github.com:paarthd00/dotfiles.git ~/dotfiles
cd ~/dotfiles
```

3. Build the CLI:

```sh
make build
```

4. Optional: install executable to your local bin:

```sh
make install
```

You can also run with cabal directly:

```sh
cabal build exe:setup-cli
```

## Haskell package

This repo is currently packaged in cabal as one executable:

- `setup-cli` (source: `setup/Main.hs`)

Use `make` for normal development:

```sh
make build
make run
make run ARGS="--theme tokyo-night --symlink"
make install
```

Or use cabal directly:

```sh
cabal build exe:setup-cli
cabal run setup-cli
cabal run setup-cli -- --theme tokyo-night --symlink
```

Convenience wrapper:

```sh
./setup-cli --theme rose-pine --symlink
```

## Running the application

Common commands:

```sh
# Full interactive flow (setup + theme)
cabal run setup-cli

# Theme-only flow
cabal run setup-cli -- --theme-only

# Non-interactive theme apply
cabal run setup-cli -- --theme tokyo-night --symlink --yes

# Wrapper equivalent
./setup-cli --theme tokyo-night --symlink
```

After theme changes, reload tmux:

```sh
tmux source-file ~/.tmux.conf \; refresh-client -S
```

## Theme selector

`setup-cli` supports:

- `night-owl`
- `rose-pine`
- `rose-pine-light`
- `tokyo-night`
- `quiet-light`

Theme assets live in `themes/<theme>/` and include:

- `alacritty.toml`
- `tmux.conf`
- `nvim-theme.lua`

Theme application details:

- `~/.config/alacritty/alacritty.toml` points to `themes/<theme>/alacritty.toml`
- `~/.tmux.conf` points to `themes/<theme>/tmux.conf`
- Codex CLI config is updated in-place (prefers `~/.code/config.toml`, falls back to `~/.codex/config.toml`; `[ui.theme]` preferred and `[tui.theme]` supported)
- `~/.claude.json` is updated in-place (`"theme": "dark"|"light-ansi"`)
- tmux tab/date colors are set in each theme's `tmux.conf` via:
  - `window-status-style`, `window-status-current-style`
  - `window-status-format`, `window-status-current-format`
  - `status-right-style`
- after changing tmux theme files, reload with:

```sh
tmux source-file ~/.tmux.conf \; refresh-client -S
```

When you run `setup-cli`, it:

- auto-detects installed tools and links only those dotfiles
- skips tools that are not installed
- includes `xmonad` only if it is installed
- does not prompt per tool during setup
- detects whether `alacritty`, `tmux`, and `nvim` are installed
- detects whether `codex` and `claude` CLIs are installed
- reports whether each config is missing, a regular file, or a symlink
- jumps directly to theme selection
- warns before replacing existing configs
- lets you either update files in place or replace them with symlinks

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

- This repository should not store personal emails, tokens, private keys, or machine-specific credentials.

## Repository layout

```text
.
├── bootstrap.sh
├── .zshrc
├── .zprofile
├── .zshenv
├── .p10k.zsh
├── .tmux.conf
├── .xmonad/
├── .config/
│   ├── nvim/
│   ├── alacritty/
```
