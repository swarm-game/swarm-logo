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

lambdaChar :: Diagram B
lambdaChar = straightBranch `atop` curvedPart
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
    sShape # lc blueColor
  , reflectY $ mShape # lc fuschiaColor
  , lambdaChar # lineCap LineCapButt # lw 10 # lc purpleColor
  , rShape # lc blueColor
  , mShape # lc fuschiaColor
  ]
  where
    -- Vaporwave colors
    -- https://www.color-hex.com/color-palette/57915
    fuschiaColor = sRGB24read "#ff00c1"
    purpleColor = sRGB24read "#9600ff"
    violetColor = sRGB24read "#4900ff"
    blueColor = sRGB24read "#00b8ff"
    cyanColor = sRGB24read "#00fff9"

main :: IO ()
main = mainWith $ composed
    # lineCap LineCapSquare . lineJoin LineJoinBevel
    # lw 5
    # frame 0.2
  