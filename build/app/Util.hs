module Util where

import Development.Shake
import Development.Shake.FilePath

artifact path = "build/out" </> path

artifactName path = makeRelative "build/out" path
