import System.Environment
import Control.Monad
import System.Directory
import System.Posix.Files

main :: IO ()
main = do
    [dir, day] <- take 2 <$> getArgs
    setCurrentDirectory dir
    createDirectoryIfMissing False day
    mapM_ (`writeFile` "") [day ++ "/input.txt", day ++ "/test.txt", "Day" ++ day ++ ".hs", "Day" ++ day ++ ".rs"]
