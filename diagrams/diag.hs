#!/usr/bin/env stack
-- stack script --resolver lts-12.21

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

node :: Int -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white <> circle 0.2 # fc green # named n

tournament :: Int -> Diagram B
tournament n = atPoints (trailVertices $ regPoly n 1) (map node [1..n]) # applyAll [connectOutside' arrowOpts j k | j<-[1..n-1], k<-[j+1..n]]

arrowOpts = with & gaps .~ small & headLength .~ local 0.15

example :: Diagram B
example = tournament 7 === hrule 5 === tournament 5

main = mainWith example
