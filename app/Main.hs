module Main (main) where
import Text.Printf (printf)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Raylib.Core (fileExists)
import System.Directory (copyFileWithMetadata, getHomeDirectory)
import Control.Monad (when)
import System.FilePath((</>))
import System.Process (callCommand)
import System.Posix.Process (getParentProcessID)
import System.Posix (signalProcess, sigTERM)
import System.IO (readFile')

profile :: IO FilePath
profile = (</> ".profile") <$> getHomeDirectory

showHelp :: IO ()
showHelp = do
    putStrLn "ALIAS𜱳\n\
    \\n\
    \Set an alias, permanently. It is written to your ~/.profile. This will start a new bash session with the alias applied\n\
    \\n\
    \Examples:\n\
    \    alias! A='alias!'\n\
    \"
    exitSuccess

keyValue :: String -> Maybe (String, String)
keyValue x = do
    -- For example: "myVar=some value" becomes this:
    --     k  : "myVar"
    --     v' : "=some value"
    --     v  : "some value"
    let (k, v') = break (== '=') x
    let (_, v) = break (/= '=') v'
    if not (null k) && not (null v) && length v' /= length v
        then Just (k, v)
        else Nothing

makeBackup :: FilePath -> IO ()
makeBackup filepath = do
    let backupFilepath = filepath ++ ".bak"
    exists <- fileExists filepath
    backupExists <- fileExists backupFilepath
    when (exists && not backupExists) $ copyFileWithMetadata filepath backupFilepath

writeAlias :: String -> String -> IO ()
writeAlias k v = do
    file <- profile
    _ <- makeBackup file
    input <- readFile' file
    let newLine = printf "alias %s='%s'" k v
    putStrLn newLine
    writeFile file (input ++ "\n" ++ newLine ++ "\n")

    -- Open a new shell to force a reload of ~/.profile.
    -- If the user closes that shell, close the calling shell as well, so
    -- the user does not notice they are using a sub-shell
    callCommand "/bin/bash"
    ppid <- getParentProcessID
    signalProcess sigTERM ppid

run :: [String] -> IO ()
run ["-h"] = showHelp
run ["--help"] = showHelp
run [] = showHelp
run [x] = do
    case keyValue x of
        Just (k, v) -> writeAlias k v
        Nothing -> putStrLn "FATAL: Malformed input."
run (_:_) = do
    putStrLn "Too many parameters! Run -h to get help"
    exitFailure

main :: IO ()
main = do
    params <- getArgs
    run params
