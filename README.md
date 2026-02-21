# dotfiles

Symlink-based dotfiles for this user.

## Managed files

- `.zshrc`
- `.zprofile`
- `.zshenv`
- `.p10k.zsh`
- `.gitconfig`
- `.config/nvim/`
- `.config/alacritty/`
- `.config/tmux/tmux.conf.local`

## Bootstrap on a new machine

```sh
git clone <your-repo-url> ~/dotfiles
cd ~/dotfiles
./bootstrap.sh
```

Notes:
- This repo intentionally excludes credentials/secrets and cache/runtime data.
- `~/.config/nvim` in this repo does not include nested git metadata.
