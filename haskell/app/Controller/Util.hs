module Controller.Util where
    
import System.Console.ANSI

clearScreenOnly :: IO ()
clearScreenOnly = do
    clearScreen
    setCursorPosition 0 0
    return ()