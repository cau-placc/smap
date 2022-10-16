-----------------------------------------------------------------------
--- Support to trace through programs
-----------------------------------------------------------------------

import System.IO
import System.IO.Unsafe(unsafePerformIO)

--- Trace a message on error stream.
errTrace :: String -> IO ()
errTrace s = hPutStrLn stderr s >> hFlush stderr

myTrace :: String -> a -> a
myTrace s x = unsafePerformIO (hPutStrLn stderr s >> hFlush stderr >> return x)
