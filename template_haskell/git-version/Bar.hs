module Bar where

import System.Process
import Language.Haskell.TH

version = (stringE . init) =<< (runIO $ readProcess "git" ["rev-parse", "HEAD"] "")
