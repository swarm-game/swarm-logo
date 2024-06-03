{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Path.Metafont

-- Vaporwave colors
-- https://www.color-hex.com/color-palette/57915
fuschiaColor = sRGB24read "#ff00c1"
purpleColor = sRGB24read "#9600ff"
violetColor = sRGB24read "#4900ff"
blueColor = sRGB24read "#00b8ff"
cyanColor = sRGB24read "#00fff9"

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
lambdaChar = centerX lineShapes `atop` deltaShape
  where
  lineShapes = straightBranch `atop` curvedPart
  deltaShape = alignB $ triangleSequence 1

  straightBranch = fromVertices [
      p2 (0, 0)
    , apexPt
    ]
  curvedPart = metafont $
    bottomRightPt .--. apexPt .--. endpt (p2 (0, 3))

  bottomRightPt = p2 (2, 0)
  apexPt = p2 (1, 2)

composedLetters :: Diagram B
composedLetters = hsep 0.5 $ map alignB [
    sShape # lc blueColor
  , reflectY $ mShape # lc fuschiaColor
  , lambdaChar # lineCap LineCapButt # lw 15 # lc purpleColor
  , rShape # lc blueColor
  , mShape # lc fuschiaColor
  ]

triangleSequence :: Int -> Diagram B
triangleSequence count = hsep 0.5 $ replicate count mkTriangle
  where
  mkTriangle = rotateBy (-(1/4)) $
    triangle 0.6 # lw 0 # fc cyanColor

fullDiagram :: Diagram B
fullDiagram = mconcat [
    alignR $ alignT (triangleSequence 3) `atop` alignT strokedLetters
  , alignR $ alignT (triangleSequence 4)
  ]
  where
  strokedLetters = composedLetters
    # lineCap LineCapSquare . lineJoin LineJoinBevel
    # lw 15
    # frame 0.2

main :: IO ()
main = mainWith $ fullDiagram # frame 0.2
  