module Main where

import Control.Exception (IOException, catch)
import Data.Char (isSpace, toLower)
import qualified System.Directory as Dir
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)
import System.IO (hFlush, stdout)

data Theme = NightOwl | RosePine | RosePineLight | TokyoNight | QuietLight deriving (Eq, Show)

data App = App
  { appName :: String,
    appExecutables :: [String],
    appTarget :: FilePath,
    appAsset :: Theme -> FilePath
  }

data Action = UpdateFile | ReplaceWithSymlink | SkipAction deriving (Eq, Show)

data ConfigStatus = Missing | RegularFile | Symlink FilePath deriving (Eq, Show)

data Options = Options
  { optTheme :: Maybe Theme,
    optYes :: Bool,
    optAction :: Maybe Action
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optTheme = Nothing,
      optYes = False,
      optAction = Nothing
    }

main :: IO ()
main = do
  args <- getArgs
  opts <- case parseArgs args of
    Left err -> die (err ++ "\nUsage: theme-cli [--theme night-owl|rose-pine|rose-pine-light|tokyo-night|quiet-light] [--yes] [--symlink|--update]")
    Right parsed -> pure parsed
  home <- Dir.getHomeDirectory
  repo <- Dir.makeAbsolute "."

  putStrLn "Theme selector for alacritty/tmux/neovim"
  putStrLn ""

  let apps = mkApps home repo

  statuses <- mapM inspectApp apps
  putStrLn "Detected tools and config state:"
  mapM_ printStatus statuses
  putStrLn ""

  chosenTheme <- case optTheme opts of
    Just t -> pure t
    Nothing -> selectTheme
  putStrLn $ "Selected theme: " ++ themeLabel chosenTheme
  putStrLn ""

  mapM_ (configureApp opts chosenTheme) statuses

  putStrLn ""
  putStrLn "Done."

mkApps :: FilePath -> FilePath -> [App]
mkApps home repo =
  [ App
      { appName = "alacritty",
        appExecutables = ["alacritty"],
        appTarget = home </> ".config" </> "alacritty" </> "alacritty.toml",
        appAsset = \t -> repo </> "themes" </> themeSlug t </> "alacritty.toml"
      },
    App
      { appName = "tmux",
        appExecutables = ["tmux"],
        appTarget = home </> ".tmux.conf",
        appAsset = \t -> repo </> "themes" </> themeSlug t </> "tmux.conf"
      },
    App
      { appName = "neovim",
        appExecutables = ["nvim"],
        appTarget = home </> ".config" </> "nvim" </> "lua" </> "paarth" </> "theme.lua",
        appAsset = \t -> repo </> "themes" </> themeSlug t </> "nvim-theme.lua"
      }
  ]

inspectApp :: App -> IO (App, Bool, ConfigStatus)
inspectApp app = do
  installed <- anyM executableExists (appExecutables app)
  status <- inspectPath (appTarget app)
  pure (app, installed, status)

printStatus :: (App, Bool, ConfigStatus) -> IO ()
printStatus (app, installed, status) = do
  let installStr = if installed then "installed" else "not installed"
  putStrLn $ "- " ++ appName app ++ ": " ++ installStr ++ ", " ++ prettyStatus status

prettyStatus :: ConfigStatus -> String
prettyStatus Missing = "config missing"
prettyStatus RegularFile = "config file exists"
prettyStatus (Symlink target) = "config symlink -> " ++ target

configureApp :: Options -> Theme -> (App, Bool, ConfigStatus) -> IO ()
configureApp opts theme (app, installed, status)
  | not installed =
      putStrLn $ "Skipping " ++ appName app ++ " (not installed)."
  | otherwise = do
      shouldConfigure <-
        if optYes opts
          then pure True
          else yesNoPrompt ("Configure " ++ appName app ++ "? [Y/n]: ") True
      if not shouldConfigure
        then putStrLn $ "Skipping " ++ appName app ++ "."
        else do
          let source = appAsset app theme
          sourceExists <- Dir.doesFileExist source
          if not sourceExists
            then putStrLn $ "Missing theme asset for " ++ appName app ++ ": " ++ source
            else do
              action <-
                case optAction opts of
                  Just forced -> pure forced
                  Nothing -> chooseAction status
              applyAction app status source action

chooseAction :: ConfigStatus -> IO Action
chooseAction status = do
  putStrLn $ "Current config: " ++ prettyStatus status
  case status of
    Missing -> do
      putStrLn "1) Create symlink"
      putStrLn "2) Create regular file"
      putStrLn "3) Skip"
      choice <- prompt "Choice [1-3] (default 1): "
      pure $ case trim choice of
        "2" -> UpdateFile
        "3" -> SkipAction
        _ -> ReplaceWithSymlink
    _ -> do
      putStrLn "1) Update file contents"
      putStrLn "2) Replace with symlink (backup current path first)"
      putStrLn "3) Skip"
      choice <- prompt "Choice [1-3] (default 2): "
      pure $ case trim choice of
        "1" -> UpdateFile
        "3" -> SkipAction
        _ -> ReplaceWithSymlink

applyAction :: App -> ConfigStatus -> FilePath -> Action -> IO ()
applyAction app status source action =
  case action of
    SkipAction -> putStrLn $ "No changes for " ++ appName app ++ "."
    ReplaceWithSymlink -> do
      ensureParent (appTarget app)
      targetExists <- Dir.doesPathExist (appTarget app)
      if targetExists
        then deleteExistingPath (appTarget app)
        else pure ()
      createSymlink source (appTarget app)
      putStrLn $ "Symlinked " ++ appName app ++ " config to " ++ source
    UpdateFile -> do
      ensureParent (appTarget app)
      case status of
        Missing -> pure ()
        Symlink destination -> putStrLn $ "Warning: target is a symlink, updating resolved file: " ++ destination
        RegularFile -> pure ()
      copyToTarget status source (appTarget app)
      putStrLn $ "Updated " ++ appName app ++ " config from " ++ source

copyToTarget :: ConfigStatus -> FilePath -> FilePath -> IO ()
copyToTarget status source target =
  case status of
    Symlink destination -> copyFileSafe source destination
    _ -> copyFileSafe source target

copyFileSafe :: FilePath -> FilePath -> IO ()
copyFileSafe source target = do
  exists <- Dir.doesPathExist target
  if exists then Dir.removeFile target else pure ()
  content <- readFile source
  writeFile target content

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

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions
  where
    go opts [] = Right opts
    go opts ("--yes" : rest) = go opts {optYes = True} rest
    go opts ("--symlink" : rest) = go opts {optAction = Just ReplaceWithSymlink} rest
    go opts ("--update" : rest) = go opts {optAction = Just UpdateFile} rest
    go opts ("--theme" : value : rest) = do
      theme <- parseTheme value
      go opts {optTheme = Just theme} rest
    go _ ("--theme" : []) = Left "Missing value for --theme"
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

themeSlug :: Theme -> String
themeSlug NightOwl = "night-owl"
themeSlug RosePine = "rose-pine"
themeSlug RosePineLight = "rose-pine-light"
themeSlug TokyoNight = "tokyo-night"
themeSlug QuietLight = "quiet-light"

themeLabel :: Theme -> String
themeLabel = themeSlug

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
        else pure RegularFile

executableExists :: String -> IO Bool
executableExists bin = do
  found <- Dir.findExecutable bin
  pure $ case found of
    Nothing -> False
    Just _ -> True

yesNoPrompt :: String -> Bool -> IO Bool
yesNoPrompt label defaultYes = do
  answer <- fmap (map toLower . trim) (prompt label)
  case answer of
    "" -> pure defaultYes
    "y" -> pure True
    "yes" -> pure True
    "n" -> pure False
    "no" -> pure False
    _ -> yesNoPrompt label defaultYes

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

ensureParent :: FilePath -> IO ()
ensureParent path = Dir.createDirectoryIfMissing True (takeDirectory path)

deleteExistingPath :: FilePath -> IO ()
deleteExistingPath path = do
  exists <- Dir.doesPathExist path
  if exists then Dir.removePathForcibly path else pure ()

createSymlink :: FilePath -> FilePath -> IO ()
createSymlink source target =
  Dir.createFileLink source target `catch` fallback
  where
    fallback :: IOException -> IO ()
    fallback _ = do
      putStrLn "Symlink creation failed, writing regular file instead."
      copyFileSafe source target
