import PA1Helper
import System.Environment (getArgs)
import GHC.Real (reduce)

-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda _ _) = lexp
id' lexp@(Apply _ _) = lexp 

-- You will need to write a reducer that does something more than
-- return whatever it was given, of course!

-- remove
remove :: (Eq a) => a -> [a] -> [a]
remove x = filter (/=x)

-- free varibales
freevars :: Lexp -> [String]
freevars (Atom s)            = [s]
freevars (Lambda v e)        = remove v (freevars e)
freevars (Apply e1 e2)       = freevars e1 ++ freevars e2

-- eta reduction
eta :: String -> Lexp -> Lexp -> Lexp 
eta x e1 e2@(Atom v)
    -- apply eta
    | x == v && x `notElem` freevars e1 = reducer e1
-- unchanged
    | otherwise = Lambda x (Apply e1 e2)
eta x e1 e2 = Lambda x (Apply e1 e2)

-- get unrepeated name
newname :: String -> [String] -> String
newname x frees
    | newx `elem` frees = newname x frees
    | otherwise = newx
    where newx = x++"1" 

-- perferom name change
change :: String -> Lexp -> Lexp -> Lexp
change x e@(Atom target) m
    |   x==target = m
    |   otherwise = e
change x e@(Apply e1 e2) m = Apply (change x e1 m) (change x e2 m)
change x e@(Lambda x1 e2) m = Lambda x1 (change x e2 m)

-- beta reduction
beta :: String -> Lexp -> Lexp -> Lexp 
-- check alpha renaming
beta x e@(Lambda lx le) m
    -- need alpha renaming
    | lx `elem` frees = beta x (Lambda newx (change lx le (Atom newx))) m
-- perferm normal change
    | otherwise = change x e m
    where frees = freevars m; newx = newname lx frees
beta x e1 e2 = change x e1 e2

reducer :: Lexp -> Lexp
-- simplest
reducer lexp@(Atom _) = lexp
-- lamdba apply eta
reducer lexp@(Lambda x e@(Apply e1 e2)) = eta x e1 (reducer e2)
-- lamdba
reducer lexp@(Lambda x e) = Lambda x (reducer e)
-- apply lambda
reducer lexp@(Apply (Lambda x e1) e2) = reducer (beta x (reducer e1) (reducer e2))
-- normal apply
reducer lexp@(Apply e1 e2) = apply (reducer e1) (reducer e2)
-- Entry point of program

-- apply helper
apply :: Lexp -> Lexp -> Lexp
apply e1@(Lambda _ _) e2 = reducer(Apply e1 e2)
apply e1 e2 = Apply e1 e2

main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram inFile outFile reducer
