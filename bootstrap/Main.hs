module Main where

import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (catchIOError)

main :: IO ()
main = do
  home <- Dir.getHomeDirectory
  envDotfiles <- lookupEnv "DOTFILES_DIR"
  dotfilesDir <- case envDotfiles of
    Just path -> pure path
    Nothing -> pure (home </> "dotfiles")

  mapM_ (uncurry (link dotfilesDir home)) links

links :: [(FilePath, FilePath)]
links =
  [ (".zshrc", ".zshrc"),
    (".zprofile", ".zprofile"),
    (".zshenv", ".zshenv"),
    (".p10k.zsh", ".p10k.zsh"),
    (".tmux.conf", ".tmux.conf"),
    (".config/alacritty", ".config/alacritty"),
    (".xmonad/xmonad.hs", ".xmonad/xmonad.hs"),
    (".xmobarrc", ".xmobarrc"),
    (".config/nvim", ".config/nvim")
  ]

link :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
link dotfilesDir home srcRel dstRel = do
  let src = dotfilesDir </> srcRel
  let dst = home </> dstRel

  Dir.createDirectoryIfMissing True (takeDirectory dst)

  exists <- Dir.doesPathExist dst
  if not exists
    then pure ()
    else do
      isLink <- Dir.pathIsSymbolicLink dst
      if isLink
        then Dir.removePathForcibly dst
        else do
          ts <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getZonedTime
          Dir.renamePath dst (dst ++ ".bak-" ++ ts)

  Dir.createDirectoryLink src dst `catchDir` Dir.createFileLink src dst
  putStrLn $ "linked " ++ dst ++ " -> " ++ src

catchDir :: IO a -> IO a -> IO a
catchDir first second = first `catchIOError` const second
