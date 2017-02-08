-- Assignment 2
-- Richard Cunard, Cunardr
-- Terrance Lee, Leete

module MiniLogo where

import Prelude hiding (Num)

type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up | Down
           deriving (Eq,Show)
 
data Expr = Val Var
          | N Num
          | Add Expr Expr
          deriving (Eq,Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving (Eq,Show)
 


 -- define line (x1, y1, x2, y2)
 --{
 -- 	pen up; 
 --		move(x1, y1);
 --     pen down;
 --		move(x2, y2);
 --}
 
line :: Cmd
line  = Define "line"  ["x1", "y1", "x2", "y2"] 
                       [Pen Up, Move (Val "x1") (Val "y1"), Pen Down, Move (Val "x2") (Val "y2")]


-- define nix (x, y, w, h)
--{
--	line(x, y, add(w, x), add(h, y));
--	line(x, add(y, h), add(x, w), y);
--}

nix :: Cmd
nix =  Define "nix" ["x", "y", "w", "h"] 
                    [Call "line" [(Val "x"), (Val "y"), (Add (Val "w") (Val "x")), (Add (Val "h") (Val "y"))], 
                     Call "line" [(Val "x"), (Add (Val "y") (Val "h")), (Add (Val "x") (Val "w")), (Val "y")]]

steps :: Int -> Prog
steps 0 = []
steps n = (steps (n - 1)) ++
         (Call "line" [(N (n-1)), (N (n-1)), (N (n-1)), (N n)]):[] ++ (Call "line" [(N (n-1)), (N (n)), (N (n)), (N n)]):[]

getMacro :: Cmd -> Macro
getMacro (Define a _ _) = a

macros :: Prog -> [Macro]
macros [] = []
macros (x:xs) = (getMacro x):[] ++ macros xs

--all following functions are used in pretty

varToStr :: [Var] -> String
varToStr [] = ")" 
varToStr (x:xs) = "'" ++ x ++ "', " ++ (varToStr xs)

cmdToStr :: Cmd -> String
cmdToStr (Define a b c) = "define " ++ a ++ "(" ++ (varToStr b) ++ "{\n" ++ (pretty c) ++ "}"
cmdToStr (Call a b) = "\t" ++ a ++ "(" ++ (exListStr b) ++ ");\n";
cmdToStr (Pen a) = if a == Down
                   then "\tpen down;\n"
                   else "\tpen up;\n"
cmdToStr (Move x y) = "\tmove (" ++ (exToStr x) ++ "," ++ (exToStr y) ++ ");\n"

exToStr :: Expr -> String
exToStr (N n) = (show n)
exToStr (Val v) = v
exToStr (Add a b) = "add(" ++ (exToStr a) ++ "," ++ (exToStr b) ++ ")"

exListStr :: [Expr] -> String
exListStr [] = ""
exListStr (x:xs) = (exToStr x) ++ "," ++ (exListStr xs)

pretty :: Prog -> String
pretty [] = ""
pretty (x:xs) = (cmdToStr x) ++ (pretty xs)

--calls pretty with putStrLn
callPret :: Prog -> IO ()
callPret a = putStrLn (pretty a)
