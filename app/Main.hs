{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Path.Metafont

sShape :: Diagram B
sShape = fromVertices $ map p2 [
      (0, 0)
    , (1, 1)
    , (0, 1)
    , (1, 2)
    ]

mShape :: Diagram B
mShape = scaleX 0.5 $ fromVertices $ map p2 [
      (0 ,0)
    , (0, 2)
    , (1, 1)
    , (2, 2)
    , (2, 0)
    ]

rShape :: Diagram B
rShape = fromVertices $ map p2 [
      (0 ,0)
    , (0, 2)
    , (1, 1)
    , (0, 1)
    , (1, 0)
    ]

example :: Diagram B
example = straightBranch `atop` curvedPart
  where
  straightBranch = fromVertices $ [
      p2 (0, 0)
    , apexPt
    ]
  curvedPart = metafont $ bottomRightPt .--. apexPt .--.endpt (p2 (0, 3))
  bottomRightPt = p2 (2, 0)
  apexPt = p2 (1, 2)

composed :: Diagram B
composed = hsep 0.2 $ map alignB [
    sShape # lc blue
  , reflectY $ mShape # lc red
  , example # lc purple
  , rShape # lc blue
  , mShape # lc red
  ]

main :: IO ()
main = mainWith $ composed
    # lineCap LineCapSquare . lineJoin LineJoinBevel
    # lw 3
    # frame 0.2
  