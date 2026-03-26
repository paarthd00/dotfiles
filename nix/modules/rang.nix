{ config, lib, pkgs, ... }:
let
  cfg = config.rang;
  repo = ../..;
  packageSet = import ../package-set.nix { inherit lib pkgs; };
  themePath = repo + "/themes/${cfg.theme}";
  lightTheme = lib.hasInfix "light" cfg.theme;
  colorFgBg = if lightTheme then "0;15" else "15;0";
  nvimSource = lib.cleanSourceWith {
    src = repo + "/home/config/nvim";
    filter = path: _type: baseNameOf path != "theme.lua";
  };
  waybarStyle = ''
    ${builtins.readFile (themePath + "/waybar-colors.css")}

    ${builtins.readFile (repo + "/window-managers/sway/waybar/style-base.css")}
  '';
  wofiStyle = ''
    ${builtins.readFile (themePath + "/wofi-colors.css")}

    ${builtins.readFile (repo + "/home/config/wofi/style-base.css")}
  '';
  selectedPackages =
    packageSet.common
    ++ lib.optionals cfg.packageSets.desktop packageSet.desktop
    ++ lib.optionals cfg.packageSets.firefox packageSet.firefox
    ++ lib.optionals cfg.packageSets.aiCli packageSet.aiCli
    ++ lib.optionals cfg.packageSets.sway packageSet.sway
    ++ lib.optionals cfg.packageSets.xmonad packageSet.xmonad
    ++ lib.optionals cfg.packageSets.fonts packageSet.fonts;
in
{
  options.rang = {
    enable = lib.mkEnableOption "rang Home Manager integration";

    theme = lib.mkOption {
      type = lib.types.enum [
        "night-owl"
        "pitch-black"
        "quiet-light"
        "rose-pine"
        "rose-pine-light"
        "tokyo-night"
      ];
      default = "night-owl";
      example = "tokyo-night";
      description = "Theme to apply across the managed configs.";
    };

    installPackages = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Install the package set through Home Manager.";
    };

    packageSets = {
      desktop = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install desktop apps like Alacritty, Ghostty, and Zed.";
      };

      firefox = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install Firefox.";
      };

      aiCli = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install Claude Code, Codex, and Opencode.";
      };

      sway = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install the Wayland/Sway stack used by this repo.";
      };

      xmonad = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install the XMonad/Xmobar stack used by this repo.";
      };

      fonts = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Install Fira Code and JetBrains Mono Nerd Font.";
      };
    };

    manage = {
      alacritty = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Manage the Alacritty config and theme file.";
      };

      ghostty = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Manage the Ghostty config and theme file.";
      };

      nvim = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Manage the Neovim config and selected theme file.";
      };

      sway = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Manage the Sway, Waybar, and Wofi configs.";
      };

      tmux = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Manage the tmux config and theme include.";
      };

      xmonad = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Manage the XMonad config and theme files.";
      };

      zsh = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Manage the zsh profile files from home/profile.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = if cfg.installPackages then lib.unique selectedPackages else [ ];

    home.sessionVariables = {
      COLORFGBG = colorFgBg;
    };

    home.file = lib.mkMerge [
      {
        ".theme-env".text = ''
          export COLORFGBG="${colorFgBg}"
        '';
      }

      (lib.optionalAttrs cfg.manage.zsh {
        ".zshrc".text = ''
          export RANG_ZSH_POWERLEVEL10K="${pkgs.zsh-powerlevel10k}/share/zsh/themes/powerlevel10k/powerlevel10k.zsh-theme"
          export RANG_ZSH_AUTOCOMPLETE="${pkgs.zsh-autocomplete}/share/zsh-autocomplete/zsh-autocomplete.plugin.zsh"
          export RANG_ZSH_AUTOSUGGESTIONS="${pkgs.zsh-autosuggestions}/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
          export RANG_ZSH_SYNTAX_HIGHLIGHTING="${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
          export RANG_FZF_COMPLETION="${pkgs.fzf}/share/fzf/completion.zsh"
          export RANG_FZF_KEY_BINDINGS="${pkgs.fzf}/share/fzf/key-bindings.zsh"

          ${builtins.readFile (repo + "/home/profile/zsh_rc.zsh")}
        '';
        ".zprofile".source = repo + "/home/profile/zsh_profile.zsh";
        ".zshenv".source = repo + "/home/profile/zsh_env.zsh";
        ".p10k.zsh".source = repo + "/home/profile/zsh_p10k.zsh";
      })

      (lib.optionalAttrs cfg.manage.tmux {
        ".tmux.conf".source = repo + "/home/profile/zsh_tmux_conf.zsh";
        ".tmux-theme.conf" = {
          source = themePath + "/tmux.conf";
          force = true;
        };
      })

      (lib.optionalAttrs cfg.manage.xmonad {
        ".xmonad/xmonad.hs".source = repo + "/window-managers/xmonad/xmonad.hs";
        ".xmonad/lib/Colors.hs" = {
          source = themePath + "/xmonad-colors.hs";
          force = true;
        };
        "xmobarrc" = {
          source = themePath + "/xmobarrc";
          force = true;
        };
      })
    ];

    xdg.configFile = lib.mkMerge [
      (lib.optionalAttrs cfg.manage.alacritty {
        "alacritty/alacritty.toml".source = repo + "/home/config/alacritty/alacritty.toml";
        "alacritty/theme.toml" = {
          source = themePath + "/alacritty.toml";
          force = true;
        };
      })

      (lib.optionalAttrs cfg.manage.ghostty {
        "ghostty/config" = {
          source = repo + "/home/config/ghostty/config";
          force = true;
        };
        "ghostty/theme" = {
          source = themePath + "/ghostty.conf";
          force = true;
        };
      })

      (lib.optionalAttrs cfg.manage.nvim {
        "nvim" = {
          source = nvimSource;
          recursive = true;
        };
        "nvim/lua/paarth/theme.lua" = {
          source = themePath + "/nvim-theme.lua";
          force = true;
        };
      })

      (lib.optionalAttrs cfg.manage.sway {
        "wofi/config".source = repo + "/home/config/wofi/config";
        "wofi/style.css" = {
          text = wofiStyle;
          force = true;
        };

        "sway/config".source = repo + "/window-managers/sway/config";
        "sway/colors.conf" = {
          source = themePath + "/sway-colors.conf";
          force = true;
        };
        "sway/outputs".source = repo + "/window-managers/sway/outputs";
        "sway/workspaces".source = repo + "/window-managers/sway/workspaces";
        "sway/scripts".source = repo + "/window-managers/sway/scripts";

        "waybar/config".source = repo + "/window-managers/sway/waybar/config";
        "waybar/quick-settings-menu.xml".source = repo + "/window-managers/sway/waybar/quick-settings-menu.xml";
        "waybar/style.css" = {
          text = waybarStyle;
          force = true;
        };
      })
    ];
  };
}
