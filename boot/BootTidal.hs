-- Haskell Tidal's own boot file (1.10.1, as shipped), with two changes:
-- Link is on, so GHCi keeps the rig's tempo, and the prompt is a marker the
-- server reads to know a block has finished.
:set -fno-warn-orphans -Wno-type-defaults -XMultiParamTypeClasses -XOverloadedStrings
:set prompt ""

import Sound.Tidal.Boot

default (Rational, Integer, Double, Pattern String)

tidalInst <- mkTidal

instance Tidally where tidal = tidalInst

enableLink

:set prompt "<<tidal>>\n"
:set prompt-cont ""
