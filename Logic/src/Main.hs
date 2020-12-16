{-# LANGUAGE ParallelListComp, PatternGuards #-}
module Main where

import System.Environment (getArgs)

-- Choose only one...
import Parser
    ( (</>),
      char,
      int,
      lower,
      many,
      runParser,
      satisfy,
      spaces,
      string,
      upper,
      Parser )

import Printer ()
import Data.List.Split ( splitOn )
import Parser ( Parser, char, many, (</>)  ) 
import Data.Maybe (isNothing)
import Debug.Trace

main = go . concat =<< mapM readFile =<< getArgs

go x = 
    case runParser ioParser x of
             []            -> putStrLn x
             ((p, _) : _)  -> putStrLn (x ++ "\n" ++ p ++ "\n\n")
--https://stackoverflow.com/questions/36937302/concatenate-a-list-of-strings-to-one-string-haskell
--http://zvon.org/other/haskell/Outputprelude/take_f.html
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Pred = Predicate String [Terms]
        |   Query String [Terms]
        |   Pred :- [Pred]
        |   Rule String Pred
        |   Percent String
            deriving(Show)
 
data Terms = Atom String
        |    Variable String
        |    Compound String [Terms]
        |    String String
        deriving(Show)

----------------------------------------------------------------------------------------------------

mercury:: Parser Pred
mercury = query </>  predicate </> comparison </> percentage

mercury'::Parser Pred
mercury'=do
    spaces
    x<-mercury
    spaces
    return x
{-
ioParser:: Parser String
ioParser = do 
    x <- many mercury'
    return (show x)
-}

ioParser:: Parser String
ioParser = do 
    x <- many mercury'
    return (applyPredstoQueries(makeSenseOf (head x) (tail x) [] [] (x))x [])

makeSenseOf :: Pred -> [Pred] -> [Pred] -> [Pred] -> [Pred] -> [Pred]
makeSenseOf (Query a b) [] c d [] = c++d 
makeSenseOf (Query a b) [] d e f= makeSenseOf (Predicate a b) [] (d ++ [Query a b]) e (tail f)
makeSenseOf _ [] c d _ = c++d
makeSenseOf (a :- b) e f g h = makeSenseOf (head e) (tail e) f (g ++ [a :- b]) h
makeSenseOf (Predicate a b) c d e f = makeSenseOf (head c) (tail c) d (e ++ [Predicate a b]) f
makeSenseOf (Query a b) c d e f = makeSenseOf (head c) (tail c) (d ++ [Query a b]) e f
makeSenseOf a b c d e = makeSenseOf (head b) (tail b) c d e 

applyPredstoQueries :: [Pred] -> [Pred] -> String -> String
applyPredstoQueries [] _ c = c
applyPredstoQueries a b c =applyPredstoQueries (tail a) b (c ++"\n"++ (applyPredstoQueries'(head a) (tail a) b) ++ "\n")

check' :: [Terms] -> [Terms] ->  [Terms] -> String -> String
check' [] [] c d
    | d == "" = "\n         %No:\n"
    | otherwise = "\n         %Yes:\n" ++ d
check' a [] c d
    | '=' `elem` (check(head a)(head c)) = d
    | otherwise = check' (tail a) (tail c) c (d ++ (check (head a)(head c)))
check' [] b c d= "\n         %No\n"
check' a b c d= check' (tail a) (tail b) c (d ++ (check (head a) (head b)))

check :: Terms -> Terms -> String
check (Atom a)(Atom b) 
    | a == b = " "
    | otherwise = ""
check (Atom a)(Variable b) = "         %"++b++"="++(a) ++ "\n"
check (Variable a) (Atom b) ="         %"++a ++ "=" ++ b ++ "\n"
check (Compound a b) (Atom c) = showCompound (Compound a b)
check (Compound a b) (Compound c d)
    | show b == show d =""
    | otherwise = showCompound (Compound a b) ++ showCompound (Compound c d)
check (Compound a b) (Variable c) = "        %"++c ++"=" ++ show(Compound a b)
check a b =(show a) ++ " " ++ (show b)
same:: Terms -> Terms -> [Terms] -> [Terms] -> Bool
same (Atom a) (Atom b) [] [] 
    | a == b = True
same a b [] _ = False
same a b _ [] = False   
same (Atom a)(Atom b) c d 
    | a == b = same(head c)(head d)(tail c)(tail d)
    | otherwise = False
same (Compound a b)(Compound c d) e f 
    | a == c = (same (head b) (head d) (tail b) (tail d))
    | otherwise = False 

showCompound:: Terms -> String
showCompound (Compound a []) = ""
showCompound (Compound a [Compound b c]) = a ++ b ++ showCompound(Compound a (tail c)) 

loop Pred [Pred]


applyPredstoQueries':: Pred -> [Pred] -> [Pred] -> String
applyPredstoQueries' (Query a b) c d = a++"("++show b++")"++check'(applyPredstoQueries'' (Query a b)(head c) d []) b b []
applyPredstoQueries' a b c = []

applyPredstoQueries'' :: Pred -> Pred -> [Pred] -> [Terms]-> [Terms]
applyPredstoQueries'' _ _ [] d = d 
applyPredstoQueries'' (Query a b) (Predicate c d :- e) f g
            | a == c = applyPredstoQueries'' (Query a b)(head f)(tail f)(unificationParser' b (Predicate c d :- e)) 
applyPredstoQueries'' (Query a b) (Predicate c d) e f 
            |       a == c  = applyPredstoQueries'' (Query a b) (head e)(tail e) (f ++ unificationParser b (Predicate c d))            
applyPredstoQueries'' (Query a b) _ e f = applyPredstoQueries'' (Query a b) (head e)(tail e) f
applyPredstoQueries'' a _ e f =  applyPredstoQueries'' a (head e) (tail e) f
----------------------------------------------------------------------------------------------------


test'':: [Terms] -> [Maybe (Substitution Terms)] ->[Terms] -> [Terms]
test'' _ [] c= c
test'' a b c = test'' a (tail b) (c++test' a (head b) [])

test':: [Terms] -> Maybe (Substitution Terms) -> [Terms]-> [Terms]
test' [] _ c= c
test' a b c =
    case b of 
        Nothing -> []
        Just d -> test' (tail a) b (c ++[(test (head a) d)])

test :: Terms -> Substitution Terms -> Terms
test (Variable a) b =
    case lookup a b of
        Nothing -> (Variable a)
        Just (Variable b)  -> (Variable b)
        Just (Atom c) -> Atom c
        Just d -> d
test (Atom a) b =
    case lookup a b of
        Nothing -> Atom a
        Just t ->  t
test (Compound a b) c =
    case lookup a c of
        Nothing -> (Compound a b)
        Just (Compound d e)  ->  Compound d e
----------------------------------------------------------------------------------------------------

unificationParser :: [Terms] -> Pred -> [Terms]
unificationParser a (Predicate b c) = test' a (unifyList a c []) []

unificationParser' :: [Terms] -> Pred -> [Terms]
unificationParser' a (Predicate b c) = a
unificationParser' a (Predicate b c :- [Predicate d f,Predicate e g]) =
    test' a (unifyList a c []) []
----------------------------------------------------------------------------------------------------
type Substitution a = [(String, a)]

unify :: Terms -> Terms -> Maybe [(String, Terms)]
unify (Atom a)(Atom b) 
    | a == b = Just []
    | otherwise = Nothing
unify (Variable a) (Atom b) = Just [(a, (Atom b))]
unify (Atom a) (Variable b) = Just [(b, (Atom a))]
unify (Variable a) (Variable b) 
    | a == b = Just []
    | otherwise = Just [(b, Variable b)]
unify (Variable a) (Compound b [c,d]) = Just [(a,(Compound b [c,d]))]
unify (Variable a) (Compound b c) = Just [(a,(Compound b c))]
unify (Compound a b)(Atom c) = Nothing
unify (Compound a b)(Compound c d) = (unifyList b d [])
unify a b = Nothing

cParse:: String -> [Terms] -> [(String,Terms)] ->Maybe [(String,Terms)]
cParse a [] c = Just c
cParse a b c = cParse a (tail b)( c++ [(a,head b)])

unifyList :: [Terms] -> [Terms] -> [(String, Terms)] -> Maybe (Substitution Terms)
unifyList [] [] c = Just c
unifyList [] b c = Just c
unifyList a [] c = Just c
unifyList (t:ts) (u:us) c= 
   case unify t u of
      Nothing -> Nothing
      Just s  -> unifyList ts us (c ++ s)
----------------------------------------------------------------------------------------------------

percentage :: Parser Pred
percentage = do
    spaces
    char '%'
    many word
    string "\n"
    return (Percent "%")
----------------------------------------------------------------------------------------------------

word:: Parser String
word = do
    char ' '
    many (satisfy('\n' /=))

predicate::Parser Pred
predicate = do
    x <- many lower
    char '(' 
    e<-many terms
    f<-term
    string ")."
    percent
    return (Predicate (show x) (e++[f]))

multPredicate::Parser Pred
multPredicate = do
    x <- many lower
    char '(' 
    e<-many terms
    f<-term
    string ")"
    return (Predicate (show x) (e++[f]))

predicates::Parser Pred
predicates = do
        x <- multPredicate
        spaces
        char ','
        spaces
        return x

----------------------------------------------------------------------------------------------------

query::Parser Pred
query = do
    x <- many lower
    char '(' 
    e<-many terms
    f<-term
    spaces
    string ")?"
    percent
    return (Query (show x) (e++[f]))

percent:: Parser String
percent = percent' </> spaces

percent':: Parser String
percent' = do
    spaces
    char '%'
    many word
    string "\n"
    return ""

    
 
----------------------------------------------------------------------------------------------------


comparison::Parser Pred
comparison = comparison' </> comparison''

comparison'::Parser Pred
comparison'=do
        x <- multPredicate
        spaces
        string ":-"
        spaces
        y <- many predicates
        z<- predicate
        return(x :- (y++[z]))  

comparison''::Parser Pred
comparison''=do
        x <- multPredicate
        spaces
        string ":-"
        spaces
        z<- multPredicate
        return(x :- [z])  


----------------------------------------------------------------------------------------------------
-- term parser 
term :: Parser Terms 
term =   compoundParser </> intParser </> varParser </> strParser </> atomParser 

terms :: Parser Terms
terms = do
        a <- term
        spaces
        char ','
        spaces
        return a

----------------------------------------------------------------------------------------------------

atomParser :: Parser Terms 
atomParser = do 
    x <- many lower 
    return (Atom x) 

----------------------------------------------------------------------------------------------------
varParser :: Parser Terms
varParser = varParser'' </> varParser'

varParser' :: Parser Terms 
varParser' = do 
    x <- upper
    return (Variable [x])

varParser'' :: Parser Terms
varParser'' = do
    x <- upper
    y<- many varOptions
    return (Variable(x:y))

varOptions :: Parser Char
varOptions = lower </> upper  

----------------------------------------------------------------------------------------------------

strParser :: Parser Terms 
strParser = do 
    char '"'
    x <- many lower
    char '"'
    return (String x)

----------------------------------------------------------------------------------------------------

compoundParser :: Parser Terms
compoundParser = compoundParser' </> compoundParser'' </> compoundParser''' </> compoundParser'''' </> compoundParser'''''

compoundParser' :: Parser Terms
compoundParser' = do
    x <- many lower
    char '('
    y <- many terms
    z <- term
    char ')'
    return (Compound x (y++[z]) )    

compoundParser'' :: Parser Terms
compoundParser'' = do
    x <- many lower
    char '('
    z <- term
    char ')'
    return (Compound x [z] )  

compoundParser''' :: Parser Terms
compoundParser''' = do
    x <- many lower
    char '['
    y <- many terms
    z <- term
    char ']'
    return (compoundConvert (y++[z]))   

compoundParser'''' :: Parser Terms
compoundParser'''' = do
    x <- many lower
    char '['
    z <- term
    char ']'
    return (compoundConvert [z])

compoundParser''''' :: Parser Terms
compoundParser''''' = do
    x <- many lower
    char '['
    y <- term
    spaces
    char '|'
    spaces
    z <- term
    spaces
    char ']'
    return (compoundConvert' ([y]++[z]))   

compoundConvert :: [Terms] -> Terms
compoundConvert x
        | null x = Atom "nil"
        | otherwise = Compound "cons" ([head x]++[compoundConvert (tail x)])

compoundConvert' :: [Terms] -> Terms
compoundConvert' x
        | null x = Atom "nil"
        | otherwise = Compound "cons" ([head x,head(tail x)])

----------------------------------------------------------------------------------------------------

intParser :: Parser Terms
intParser = do 
    x <- int
    return (intConvert x)

intConvert :: Int -> Terms
intConvert x 
            | x==0 = Atom "zero"
            | x>0  = Compound "succ" [(intConvert (x-1))]


----------------------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------------------------------------------------------------