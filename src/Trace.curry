-----------------------------------------------------------------------
--- Support to trace through programs
-----------------------------------------------------------------------

import IO
import Unsafe(unsafePerformIO)

--- Trace a message on error stream.
errTrace s = hPutStrLn stderr s >> hFlush stderr

myTrace s x = unsafePerformIO (hPutStrLn stderr s >> hFlush stderr >> return x)
