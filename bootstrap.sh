#!/usr/bin/env sh
set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)"
DOTFILES_DIR="${DOTFILES_DIR:-$SCRIPT_DIR}"
THEMES_DIR="$DOTFILES_DIR/themes"

THEME="${THEME:-}"
ASSUME_YES=0
BACKUP_EXT="${RANG_BACKUP_EXT:-rang-backup}"

log() { printf '%s\n' "$*"; }
has_cmd() { command -v "$1" >/dev/null 2>&1; }
wm_is_running() { pgrep -x "$1" >/dev/null 2>&1; }

is_repo_managed_symlink() {
  path="$1"
  [ -L "$path" ] || return 1

  link="$(readlink "$path" 2>/dev/null || true)"
  case "$link" in
    "$DOTFILES_DIR"/*|../WM-Theme/*|../rang/*|*/WM-Theme/*|*/rang/*) return 0 ;;
  esac

  if has_cmd realpath; then
    resolved="$(realpath -m "$path" 2>/dev/null || true)"
    case "$resolved" in
      "$DOTFILES_DIR"/*) return 0 ;;
    esac
  fi

  return 1
}

usage() {
  cat <<EOF
Usage: ./bootstrap.sh [options]

Applies the rang Home Manager setup and selects a theme.

  --theme THEME    Apply a specific theme directly
  --yes            Non-interactive (default theme: night-owl)
  -h, --help
EOF
}

require_theme_assets() {
  chosen="$1"
  for rel in alacritty.toml ghostty.conf nvim-theme.lua sway-colors.conf tmux.conf waybar-colors.css wofi-colors.css xmobarrc xmonad-colors.hs; do
    [ -f "$THEMES_DIR/$chosen/$rel" ] || {
      log "error: missing theme asset: $THEMES_DIR/$chosen/$rel"
      exit 1
    }
  done
}

choose_theme() {
  if [ -n "$THEME" ]; then
    require_theme_assets "$THEME"
    return 0
  fi

  if [ "$ASSUME_YES" -eq 1 ] || [ ! -t 0 ]; then
    THEME="night-owl"
    require_theme_assets "$THEME"
    return 0
  fi

  log "Select a rang theme:"
  choices=""
  for d in "$THEMES_DIR"/*; do
    [ -d "$d" ] || continue
    name="$(basename "$d")"
    choices="${choices}
${name}"
  done
  choices="$(printf '%s\n' "$choices" | sed '/^$/d' | sort)"
  [ -n "$choices" ] || {
    log "error: no themes found in $THEMES_DIR"
    exit 1
  }

  idx=1
  default_idx=1
  for name in $choices; do
    [ "$name" = "night-owl" ] && default_idx="$idx"
    log "  $idx) $name"
    idx=$((idx + 1))
  done
  printf "Choice [1-%s] (default %s): " "$((idx - 1))" "$default_idx"
  IFS= read -r input
  input="${input:-$default_idx}"

  if [ "$input" -ge 1 ] 2>/dev/null && [ "$input" -lt "$idx" ] 2>/dev/null; then
    sel=1
    for name in $choices; do
      [ "$sel" -eq "$input" ] && {
        THEME="$name"
        break
      }
      sel=$((sel + 1))
    done
  else
    THEME="$input"
  fi

  require_theme_assets "$THEME"
}

migrate_legacy_links() {
  log "Checking for legacy repo-managed config links"

  for path in \
    "$HOME/.config/alacritty" \
    "$HOME/.config/nvim" \
    "$HOME/.config/sway" \
    "$HOME/.config/waybar" \
    "$HOME/.config/wofi"
  do
    if is_repo_managed_symlink "$path"; then
      log "  removing legacy link: $path -> $(readlink "$path")"
      rm -f "$path"
    fi
  done
}

apply_home_manager() {
  export RANG_THEME="$THEME"

  if has_cmd home-manager; then
    home-manager switch -b "$BACKUP_EXT" --impure --flake "path:$DOTFILES_DIR#default"
    return 0
  fi

  nix run github:nix-community/home-manager -- switch -b "$BACKUP_EXT" --impure --flake "path:$DOTFILES_DIR#default"
}

reload_runtime() {
  log ""
  log "Reloading live programs for theme: $THEME"

  if has_cmd swaymsg && wm_is_running sway; then
    swaymsg reload >/dev/null 2>&1 && log "  reloaded: sway" || log "  warn: sway reload failed"
  fi

  if has_cmd tmux && tmux info >/dev/null 2>&1; then
    tmux source-file ~/.tmux.conf \; refresh-client -S >/dev/null 2>&1 \
      && log "  reloaded: tmux" \
      || log "  warn: tmux reload failed"
  fi

  if has_cmd xmonad && wm_is_running xmonad; then
    if xmonad --recompile >/dev/null 2>&1; then
      xmonad --restart >/dev/null 2>&1 && log "  reloaded: xmonad" || log "  warn: xmonad restart failed"
    else
      log "  warn: xmonad recompile failed"
    fi
  fi

  if [ -f "$HOME/.config/alacritty/alacritty.toml" ]; then
    touch "$HOME/.config/alacritty/alacritty.toml" "$HOME/.config/alacritty/theme.toml" 2>/dev/null || true
    log "  nudged: alacritty"
  fi

  if [ -f "$HOME/.config/ghostty/config" ]; then
    touch "$HOME/.config/ghostty/config" "$HOME/.config/ghostty/theme" 2>/dev/null || true
    log "  nudged: ghostty"
  fi
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --theme)
      shift
      [ "$#" -gt 0 ] || {
        log "error: --theme requires a value"
        exit 1
      }
      THEME="$1"
      ;;
    --yes) ASSUME_YES=1 ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      log "error: unknown argument: $1"
      usage
      exit 1
      ;;
  esac
  shift
done

choose_theme
log "Applying rang with theme: $THEME"
migrate_legacy_links
apply_home_manager
reload_runtime
log ""
log "rang applied."
