module Main where

import Control.Exception (IOException, catch)
import Control.Concurrent (threadDelay)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, findIndex, isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified System.Directory as Dir
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), die)
import System.FilePath ((</>), isRelative, takeDirectory)
import System.IO.Error (isAlreadyInUseError)
import System.IO (hFlush, stdout)
import System.Process (readProcessWithExitCode)

data Theme = NightOwl | RosePine | RosePineLight | TokyoNight | QuietLight deriving (Eq, Show)

data Action = UpdateFile | ReplaceWithSymlink | SkipAction deriving (Eq, Show)

data ConfigStatus = Missing | RegularFile | Directory | Symlink FilePath deriving (Eq, Show)

data SetupItem = SetupItem
  { setupName :: String,
    setupSourceRel :: FilePath,
    setupTargetRel :: FilePath,
    setupExecutables :: [String],
    setupOptional :: Bool
  }

data ThemeApp = ThemeApp
  { themeAppName :: String,
    themeExecutables :: [String],
    themeTarget :: FilePath,
    themeAsset :: Theme -> FilePath
  }

data CliThemeApp = CliThemeApp
  { cliThemeAppName :: String,
    cliThemeExecutables :: [String],
    cliThemeTarget :: FilePath,
    cliThemeApply :: Theme -> FilePath -> IO ()
  }

data Options = Options
  { optTheme :: Maybe Theme,
    optYes :: Bool,
    optAction :: Maybe Action,
    optSetupOnly :: Bool,
    optThemeOnly :: Bool,
    optDotfilesDir :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optTheme = Nothing,
      optYes = False,
      optAction = Nothing,
      optSetupOnly = False,
      optThemeOnly = False,
      optDotfilesDir = Nothing
    }

main :: IO ()
main = do
  args <- getArgs
  opts <- case parseArgs args of
    Left err -> die (err ++ "\n" ++ usage)
    Right parsed -> pure parsed

  home <- Dir.getHomeDirectory
  envDotfiles <- lookupEnv "DOTFILES_DIR"
  cwd <- Dir.makeAbsolute "."
  let dotfilesDir = fromMaybe (fromMaybe (home </> "dotfiles") envDotfiles) (optDotfilesDir opts)

  if optThemeOnly opts
    then pure ()
    else runSetup dotfilesDir home opts

  if optSetupOnly opts
    then pure ()
    else runThemeFlow dotfilesDir home opts cwd

  putStrLn ""
  putStrLn "Done."

runSetup :: FilePath -> FilePath -> Options -> IO ()
runSetup dotfilesDir home _opts = do
  putStrLn "Dotfiles setup"
  putStrLn ""
  mapM_ (applySetupItem dotfilesDir home) setupItems
  putStrLn ""

setupItems :: [SetupItem]
setupItems =
  [ SetupItem "zshrc" ".zshrc" ".zshrc" ["zsh"] False,
    SetupItem "tmux" ".tmux.conf" ".tmux.conf" ["tmux"] False,
    SetupItem "alacritty" ".config/alacritty" ".config/alacritty" ["alacritty"] False,
    SetupItem "neovim" ".config/nvim" ".config/nvim" ["nvim"] False,
    SetupItem "xmonad" ".xmonad/xmonad.hs" ".xmonad/xmonad.hs" ["xmonad"] True
  ]

applySetupItem :: FilePath -> FilePath -> SetupItem -> IO ()
applySetupItem dotfilesDir home item = do
  let src = dotfilesDir </> setupSourceRel item
  let dst = home </> setupTargetRel item

  installed <- anyM executableExists (setupExecutables item)
  if not installed
    then putStrLn $ "Skipping " ++ setupName item ++ " (not installed)"
    else do
      srcExists <- Dir.doesPathExist src
      if not srcExists
        then putStrLn $ "Skipping " ++ setupName item ++ " (source missing: " ++ src ++ ")"
        else do
          status <- inspectPath dst
          putStrLn $ "- " ++ setupName item ++ optionalSuffix item ++ ": " ++ prettyStatus status
          srcResolved <- Dir.canonicalizePath src
          case status of
            Symlink targetResolved | targetResolved == srcResolved ->
              putStrLn $ "Already linked " ++ dst ++ " -> " ++ src
            _ -> do
              Dir.createDirectoryIfMissing True (takeDirectory dst)
              exists <- Dir.doesPathExist dst
              if exists
                then deleteExistingPath dst
                else pure ()
              createSymlinkPath src dst
              putStrLn $ "Linked " ++ dst ++ " -> " ++ src

optionalSuffix :: SetupItem -> String
optionalSuffix item =
  if setupOptional item then " (optional)" else ""

runThemeFlow :: FilePath -> FilePath -> Options -> FilePath -> IO ()
runThemeFlow dotfilesDir home opts cwd = do
  let repo = if isRelative dotfilesDir then cwd </> dotfilesDir else dotfilesDir
  let apps = mkThemeApps home repo
  cliApps <- mkCliThemeApps home

  putStrLn "Theme setup"
  putStrLn ""

  statuses <- mapM inspectThemeApp apps
  cliStatuses <- mapM inspectCliThemeApp cliApps
  putStrLn "Detected tools and config state:"
  mapM_ printThemeStatus statuses
  mapM_ printCliThemeStatus cliStatuses
  putStrLn ""

  chosenTheme <- case optTheme opts of
    Just t -> pure t
    Nothing -> selectTheme
  putStrLn $ "Selected theme: " ++ themeLabel chosenTheme
  putStrLn ""
  mapM_ (configureThemeApp opts chosenTheme) statuses
  mapM_ (configureCliThemeApp chosenTheme) cliStatuses
  putStrLn ""

mkThemeApps :: FilePath -> FilePath -> [ThemeApp]
mkThemeApps home repo =
  [ ThemeApp
      { themeAppName = "alacritty",
        themeExecutables = ["alacritty"],
        themeTarget = home </> ".config" </> "alacritty" </> "alacritty.toml",
        themeAsset = \t -> repo </> "themes" </> themeSlug t </> "alacritty.toml"
      },
    ThemeApp
      { themeAppName = "tmux",
        themeExecutables = ["tmux"],
        themeTarget = home </> ".tmux.conf",
        themeAsset = \t -> repo </> "themes" </> themeSlug t </> "tmux.conf"
      },
    ThemeApp
      { themeAppName = "neovim",
        themeExecutables = ["nvim"],
        themeTarget = home </> ".config" </> "nvim" </> "lua" </> "paarth" </> "theme.lua",
        themeAsset = \t -> repo </> "themes" </> themeSlug t </> "nvim-theme.lua"
      }
  ]

mkCliThemeApps :: FilePath -> IO [CliThemeApp]
mkCliThemeApps home = do
  codexTarget <- detectCodexThemeTarget home
  pure
    [ CliThemeApp
        { cliThemeAppName = "codex",
          cliThemeExecutables = ["codex"],
          cliThemeTarget = codexTarget,
          cliThemeApply = applyCodexTheme
        },
      CliThemeApp
        { cliThemeAppName = "claude",
          cliThemeExecutables = ["claude"],
          cliThemeTarget = home </> ".claude.json",
          cliThemeApply = applyClaudeTheme
        }
    ]

detectCodexThemeTarget :: FilePath -> IO FilePath
detectCodexThemeTarget home = do
  let modern = home </> ".code" </> "config.toml"
  let legacy = home </> ".codex" </> "config.toml"
  modernExists <- Dir.doesPathExist modern
  legacyExists <- Dir.doesPathExist legacy
  pure $
    if modernExists
      then modern
      else
        if legacyExists
          then legacy
          else modern

inspectThemeApp :: ThemeApp -> IO (ThemeApp, Bool, ConfigStatus)
inspectThemeApp app = do
  installed <- anyM executableExists (themeExecutables app)
  status <- inspectPath (themeTarget app)
  pure (app, installed, status)

inspectCliThemeApp :: CliThemeApp -> IO (CliThemeApp, Bool, ConfigStatus)
inspectCliThemeApp app = do
  installed <- anyM executableExists (cliThemeExecutables app)
  status <- inspectPath (cliThemeTarget app)
  pure (app, installed, status)

printThemeStatus :: (ThemeApp, Bool, ConfigStatus) -> IO ()
printThemeStatus (app, installed, status) = do
  let installStr = if installed then "installed" else "not installed"
  putStrLn $ "- " ++ themeAppName app ++ ": " ++ installStr ++ ", " ++ prettyStatus status

printCliThemeStatus :: (CliThemeApp, Bool, ConfigStatus) -> IO ()
printCliThemeStatus (app, installed, status) = do
  let installStr = if installed then "installed" else "not installed"
  putStrLn $ "- " ++ cliThemeAppName app ++ " (cli theme): " ++ installStr ++ ", " ++ prettyStatus status

configureThemeApp :: Options -> Theme -> (ThemeApp, Bool, ConfigStatus) -> IO ()
configureThemeApp opts theme (app, installed, status)
  | not installed =
      putStrLn $ "Skipping " ++ themeAppName app ++ " (not installed)."
  | otherwise = do
      let source = themeAsset app theme
      sourceExists <- Dir.doesFileExist source
      if not sourceExists
        then putStrLn $ "Missing theme asset for " ++ themeAppName app ++ ": " ++ source
        else do
          let action = fromMaybe ReplaceWithSymlink (optAction opts)
          applyThemeAction app status source action

configureCliThemeApp :: Theme -> (CliThemeApp, Bool, ConfigStatus) -> IO ()
configureCliThemeApp theme (app, installed, status)
  | not installed =
      putStrLn $ "Skipping " ++ cliThemeAppName app ++ " cli theme (not installed)."
  | status == Directory =
      putStrLn $ "Skipping " ++ cliThemeAppName app ++ " cli theme (target is a directory)."
  | otherwise = do
      ensureParent (cliThemeTarget app)
      result <-
        (cliThemeApply app theme (cliThemeTarget app) >> pure (Nothing :: Maybe IOException))
          `catch` \err -> pure (Just err)
      case result of
        Nothing ->
          putStrLn $ "Updated " ++ cliThemeAppName app ++ " cli theme."
        Just err ->
          putStrLn $
            "Warning: could not update "
              ++ cliThemeAppName app
              ++ " cli theme: "
              ++ show err

applyThemeAction :: ThemeApp -> ConfigStatus -> FilePath -> Action -> IO ()
applyThemeAction app status source action =
  case action of
    SkipAction -> putStrLn $ "No changes for " ++ themeAppName app ++ "."
    ReplaceWithSymlink -> do
      sourceResolved <- Dir.canonicalizePath source
      case status of
        Symlink targetResolved | targetResolved == sourceResolved -> do
          putStrLn $ themeAppName app ++ " config already points to " ++ source
          postThemeApply app
        _ -> do
          ensureParent (themeTarget app)
          targetExists <- Dir.doesPathExist (themeTarget app)
          if targetExists
            then deleteExistingPath (themeTarget app)
            else pure ()
          createFileSymlink source (themeTarget app)
          putStrLn $ "Symlinked " ++ themeAppName app ++ " config to " ++ source
          postThemeApply app
    UpdateFile -> do
      ensureParent (themeTarget app)
      case status of
        Missing -> pure ()
        Symlink destination -> putStrLn $ "Warning: target is a symlink, updating resolved file: " ++ destination
        _ -> pure ()
      copyToTarget status source (themeTarget app)
      putStrLn $ "Updated " ++ themeAppName app ++ " config from " ++ source
      postThemeApply app

postThemeApply :: ThemeApp -> IO ()
postThemeApply app =
  if themeAppName app == "tmux"
    then refreshTmux (themeTarget app)
    else pure ()

refreshTmux :: FilePath -> IO ()
refreshTmux tmuxConf = do
  tmuxEnv <- lookupEnv "TMUX"
  let tmuxArgs =
        case tmuxSocketFromEnv tmuxEnv of
          Just socket -> ["-S", socket]
          Nothing -> []
  sourceResult <- readProcessWithExitCode "tmux" (tmuxArgs ++ ["source-file", tmuxConf]) ""
  case sourceResult of
    (ExitSuccess, _, _) -> do
      _ <- readProcessWithExitCode "tmux" (tmuxArgs ++ ["refresh-client", "-S"]) ""
      putStrLn "Refreshed tmux configuration."
    _ ->
      putStrLn "tmux config updated; start/reload tmux server to see changes."

tmuxSocketFromEnv :: Maybe String -> Maybe String
tmuxSocketFromEnv Nothing = Nothing
tmuxSocketFromEnv (Just raw) =
  case break (== ',') raw of
    ("", _) -> Nothing
    (socket, _) -> Just socket

copyToTarget :: ConfigStatus -> FilePath -> FilePath -> IO ()
copyToTarget status source target =
  case status of
    Symlink destination -> copyFileSafe source destination
    _ -> copyFileSafe source target

applyCodexTheme :: Theme -> FilePath -> IO ()
applyCodexTheme theme target = do
  let desired = codexThemeName theme
  exists <- Dir.doesPathExist target
  if not exists
    then writeFileWithRetry target (codexThemeSection desired)
    else do
      content <- readFile target
      writeFileWithRetry target (setCodexThemeName desired content)

applyClaudeTheme :: Theme -> FilePath -> IO ()
applyClaudeTheme theme target = do
  let desired = claudeThemeName theme
  exists <- Dir.doesPathExist target
  if not exists
    then writeFileWithRetry target ("{\n  \"theme\": \"" ++ desired ++ "\"\n}\n")
    else do
      content <- readFile target
      writeFileWithRetry target (setClaudeTheme desired content)

codexThemeName :: Theme -> String
codexThemeName QuietLight = "light"
codexThemeName RosePineLight = "light"
codexThemeName TokyoNight = "dark-carbon-night"
codexThemeName _ = "dark"

claudeThemeName :: Theme -> String
claudeThemeName QuietLight = "light-ansi"
claudeThemeName RosePineLight = "light-ansi"
claudeThemeName _ = "dark"

codexThemeSectionWith :: String -> String -> String
codexThemeSectionWith section name = "[" ++ section ++ "]\nname = \"" ++ name ++ "\"\n"

codexThemeSection :: String -> String
codexThemeSection = codexThemeSectionWith "ui.theme"

setCodexThemeName :: String -> String -> String
setCodexThemeName name content =
  if "[ui.theme]" `isInfixOf` content
    then setCodexThemeNameInSection "ui.theme" name content
    else
      if "[tui.theme]" `isInfixOf` content
        then setCodexThemeNameInSection "tui.theme" name content
        else
          let base = if null content then "" else ensureTrailingNewline content
           in base ++ "\n" ++ codexThemeSection name

setCodexThemeNameInSection :: String -> String -> String -> String
setCodexThemeNameInSection section name content =
  unlines (go False False (lines content))
  where
    go :: Bool -> Bool -> [String] -> [String]
    go inSection seen [] =
      if inSection && not seen then ["name = \"" ++ name ++ "\""] else []
    go inSection seen (line : rest)
      | isTargetSection section line =
          line : go True False rest
      | inSection && isSectionHeader line =
          (if seen then [] else ["name = \"" ++ name ++ "\""]) ++ [line] ++ go False False rest
      | inSection && isNameAssignment line =
          ("name = \"" ++ name ++ "\"") : go True True rest
      | otherwise =
          line : go inSection seen rest

setClaudeTheme :: String -> String -> String
setClaudeTheme name content =
  let ls = lines content
   in case findIndex isThemeLine ls of
        Just idx ->
          let updated = replaceAt idx (replaceThemeLine name (ls !! idx)) ls
           in ensureTrailingNewline (unlines updated)
        Nothing ->
          case findLastIndex isClosingBrace ls of
            Just closeIdx ->
              let withComma = ensurePriorComma closeIdx ls
                  withTheme = insertAt closeIdx ("  \"theme\": \"" ++ name ++ "\"") withComma
               in ensureTrailingNewline (unlines withTheme)
            Nothing ->
              "{\n  \"theme\": \"" ++ name ++ "\"\n}\n"

isTargetSection :: String -> String -> Bool
isTargetSection section line = trim line == "[" ++ section ++ "]"

isSectionHeader :: String -> Bool
isSectionHeader line =
  let t = trim line
   in not (null t) && head t == '[' && last t == ']'

isNameAssignment :: String -> Bool
isNameAssignment line =
  let t = trim line
   in "name" `isPrefixOf` t && '=' `elem` t

isThemeLine :: String -> Bool
isThemeLine line =
  let t = trim line
   in "\"theme\"" `isPrefixOf` t && ':' `elem` t

replaceThemeLine :: String -> String -> String
replaceThemeLine name oldLine =
  let indent = takeWhile isSpace oldLine
      trailingComma = if hasTrailingComma oldLine then "," else ""
   in indent ++ "\"theme\": \"" ++ name ++ "\"" ++ trailingComma

isClosingBrace :: String -> Bool
isClosingBrace line = trim line == "}"

hasTrailingComma :: String -> Bool
hasTrailingComma line =
  let trimmed = trimRight line
   in not (null trimmed) && last trimmed == ','

trimRight :: String -> String
trimRight = dropWhileEnd isSpace

findLastIndex :: (a -> Bool) -> [a] -> Maybe Int
findLastIndex p xs =
  case reverse (zip [0 ..] xs) of
    [] -> Nothing
    pairs ->
      case filter (p . snd) pairs of
        ((idx, _) : _) -> Just idx
        [] -> Nothing

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx value xs =
  let (before, after) = splitAt idx xs
   in case after of
        [] -> xs
        (_ : rest) -> before ++ (value : rest)

insertAt :: Int -> a -> [a] -> [a]
insertAt idx value xs =
  let (before, after) = splitAt idx xs
   in before ++ [value] ++ after

ensurePriorComma :: Int -> [String] -> [String]
ensurePriorComma closeIdx xs =
  case findPreviousNonEmpty closeIdx xs of
    Nothing -> xs
    Just idx ->
      let line = xs !! idx
          t = trim line
       in if t == "{" || hasTrailingComma line
            then xs
            else replaceAt idx (trimRight line ++ ",") xs

findPreviousNonEmpty :: Int -> [String] -> Maybe Int
findPreviousNonEmpty start xs =
  let candidates = reverse [0 .. start - 1]
   in case filter (\idx -> not (null (trim (xs !! idx)))) candidates of
        (idx : _) -> Just idx
        [] -> Nothing

ensureTrailingNewline :: String -> String
ensureTrailingNewline s =
  if null s || last s == '\n' then s else s ++ "\n"

writeFileWithRetry :: FilePath -> String -> IO ()
writeFileWithRetry target content = go (5 :: Int)
  where
    go attempts =
      writeFile target content
        `catch` \err ->
          if attempts > 1 && isLockError err
            then do
              threadDelay 200000
              go (attempts - 1)
            else ioError err

isLockError :: IOError -> Bool
isLockError err =
  isAlreadyInUseError err
    || "resource busy" `isInfixOf` lower
    || "file is locked" `isInfixOf` lower
  where
    lower = map toLower (show err)

copyFileSafe :: FilePath -> FilePath -> IO ()
copyFileSafe source target = do
  exists <- Dir.doesPathExist target
  if exists then Dir.removeFile target else pure ()
  content <- readFile source
  writeFile target content

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts []
      | optSetupOnly opts && optThemeOnly opts = Left "Cannot use --setup-only and --theme-only together"
      | otherwise = Right opts
    go opts ("--yes" : rest) = go opts {optYes = True} rest
    go opts ("--symlink" : rest) = go opts {optAction = Just ReplaceWithSymlink} rest
    go opts ("--update" : rest) = go opts {optAction = Just UpdateFile} rest
    go opts ("--setup-only" : rest) = go opts {optSetupOnly = True} rest
    go opts ("--theme-only" : rest) = go opts {optThemeOnly = True} rest
    go opts ("--theme" : value : rest) = do
      theme <- parseTheme value
      go opts {optTheme = Just theme} rest
    go opts ("--dotfiles-dir" : value : rest) = go opts {optDotfilesDir = Just value} rest
    go _ ("--theme" : []) = Left "Missing value for --theme"
    go _ ("--dotfiles-dir" : []) = Left "Missing value for --dotfiles-dir"
    go _ (flag : _) = Left $ "Unknown argument: " ++ flag

parseTheme :: String -> Either String Theme
parseTheme raw =
  case map toLower (trim raw) of
    "night-owl" -> Right NightOwl
    "nightowl" -> Right NightOwl
    "rose-pine" -> Right RosePine
    "rosepine" -> Right RosePine
    "rose-pine-dark" -> Right RosePine
    "rosepinedark" -> Right RosePine
    "rose-pine-light" -> Right RosePineLight
    "rosepinelight" -> Right RosePineLight
    "rose-pine-dawn" -> Right RosePineLight
    "rosepinedawn" -> Right RosePineLight
    "tokyo-night" -> Right TokyoNight
    "tokyonight" -> Right TokyoNight
    "quiet-light" -> Right QuietLight
    "quietlight" -> Right QuietLight
    _ -> Left $ "Unsupported theme: " ++ raw

selectTheme :: IO Theme
selectTheme = do
  putStrLn "Select a theme:"
  putStrLn "1) night-owl"
  putStrLn "2) rose-pine"
  putStrLn "3) rose-pine-light"
  putStrLn "4) tokyo-night"
  putStrLn "5) quiet-light"
  choice <- prompt "Choice [1-5] (default 1): "
  pure $ case trim choice of
    "2" -> RosePine
    "3" -> RosePineLight
    "4" -> TokyoNight
    "5" -> QuietLight
    _ -> NightOwl

themeSlug :: Theme -> String
themeSlug NightOwl = "night-owl"
themeSlug RosePine = "rose-pine"
themeSlug RosePineLight = "rose-pine-light"
themeSlug TokyoNight = "tokyo-night"
themeSlug QuietLight = "quiet-light"

themeLabel :: Theme -> String
themeLabel = themeSlug

usage :: String
usage =
  unlines
    [ "Usage: setup [options]",
      "  --yes                                non-interactive defaults",
      "  --theme night-owl|rose-pine|rose-pine-light|tokyo-night|quiet-light",
      "  --symlink                            force symlink mode for themed apps",
      "  --update                             force update-file mode for themed apps",
      "  --setup-only                         run only dotfiles linking",
      "  --theme-only                         run only theme setup",
      "  --dotfiles-dir PATH                  dotfiles repo path"
    ]

inspectPath :: FilePath -> IO ConfigStatus
inspectPath path = do
  exists <- Dir.doesPathExist path
  if not exists
    then pure Missing
    else do
      isLink <- Dir.pathIsSymbolicLink path
      if isLink
        then do
          resolved <- Dir.canonicalizePath path
          pure (Symlink resolved)
        else do
          isDir <- Dir.doesDirectoryExist path
          if isDir then pure Directory else pure RegularFile

prettyStatus :: ConfigStatus -> String
prettyStatus Missing = "missing"
prettyStatus RegularFile = "regular file"
prettyStatus Directory = "directory"
prettyStatus (Symlink target) = "symlink -> " ++ target

executableExists :: String -> IO Bool
executableExists bin = do
  found <- Dir.findExecutable bin
  pure $ case found of
    Nothing -> False
    Just _ -> True

ensureParent :: FilePath -> IO ()
ensureParent path = Dir.createDirectoryIfMissing True (takeDirectory path)

deleteExistingPath :: FilePath -> IO ()
deleteExistingPath path = do
  exists <- Dir.doesPathExist path
  if exists then Dir.removePathForcibly path else pure ()

createSymlinkPath :: FilePath -> FilePath -> IO ()
createSymlinkPath source target = do
  isDir <- Dir.doesDirectoryExist source
  if isDir
    then Dir.createDirectoryLink source target `catch` fallback
    else createFileSymlink source target
  where
    fallback :: IOException -> IO ()
    fallback _ = putStrLn $ "Could not symlink directory: " ++ source

createFileSymlink :: FilePath -> FilePath -> IO ()
createFileSymlink source target =
  Dir.createFileLink source target `catch` fallback
  where
    fallback :: IOException -> IO ()
    fallback _ = do
      putStrLn "Symlink creation failed, writing regular file instead."
      copyFileSafe source target

prompt :: String -> IO String
prompt label = do
  putStr label
  hFlush stdout
  getLine

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM _ [] = pure False
anyM f (x : xs) = do
  result <- f x
  if result then pure True else anyM f xs
