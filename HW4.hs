module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState
import KarelExamples

test (Not t)    w r = not (test t w r)
test (Facing c) _ r = c == getFacing r
test (Clear d)  w r = isClear (relativePos d r) w
test (Beeper)   w r = hasBeeper (getPos r) w
test (Empty)    _ r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown       _ _ r = Done r
stmt Move           _ w r = let p = relativePos Front r
                            in if isClear p w then OK w (setPos p r)
                                              else Error ("Blocked at: " ++ show p)
stmt PickBeeper     _ w r = let p = getPos r
                            in if hasBeeper p w
                                  then OK (decBeeper p w) (incBag r)
                                  else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper      _ w r = let p = getPos r
                            in if isEmpty r then Error "No beeper to put."
                                            else OK (incBeeper p w) (decBag r)
stmt (Turn d)       _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Block [])     d w r = OK w r
stmt (Block (s:ss)) d w r = case stmt s d w r of
                              (OK w' r') -> stmt (Block ss) d w' r'
                              (Done r') -> Done r'
                              (Error m) -> Error m
stmt (If t bt be)   d w r = if test t w r then stmt bt d w r
                                          else stmt be d w r
stmt (Call m)       d w r = case lookup m d of
                              (Just b) -> stmt b d w r
                              _        -> Error ("Undefined macro: " ++ m)
stmt (Iterate i b)  d w r = if i > 0 then case stmt b d w r of
                                            (OK w' r') -> stmt (Iterate (i-1) b) d w' r'
                                            (Done r') -> Done r'
                                            (Error m) -> Error m
                                     else OK w r
stmt (While t b)    d w r = if test t w r then case stmt b d w r of
                                                 (OK w' r') -> stmt (While t b) d w' r'
                                                 (Done r') -> Done r'
                                                 (Error m) -> Error m
                                          else OK w r

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r	
