. "$HOME/.cargo/env"
[ -f "$HOME/.theme-env" ] && . "$HOME/.theme-env"

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then . "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi # added by Nix installer
