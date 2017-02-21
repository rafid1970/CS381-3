module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState

getCard :: Robot -> Card
getCard (_, card, _) = card

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test = (Not theTest) world robot =
not (test theTest world robot)

test (Facing cardinalDir) _ (_, robotFacingDir, _)=
cardinalDir == robotFacingDir

test(Clear relativeDir) world (robotPos, robotFacingDir, _)=

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState

getCard :: Robot -> Card
getCard (_, card, _) = card

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test = (Not theTest) world robot =
not (test theTest world robot)

test (Facing cardinalDir) _ (_, robotFacingDir, _)=
cardinalDir == robotFacingDir

test(Clear relativeDir) world (robotPos, robotFacingDir, _)=

test Beeper world

test Empty

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
