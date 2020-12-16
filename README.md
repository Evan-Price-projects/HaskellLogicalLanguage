Evan Price and Rohan Gupta

Logic Path

Work Cited:

Functionality:

mercury::Parser String
mercury= do
        x<-Predicate
        y<-Term
        z<-Ending
        termGenerator x y z


termGenerator:: Parser Pred -> Parser Term -> Parser Char -> Parser String
termGenerator = undefined

data Pred = Var String
        | Addn Term Term Term
        | Multn Term Term Term
        | Eqi Pred Pred
        | Addi Pred Pred 
        | Multi Pred Pred Pred
        | Inverse Pred Pred Pred
        | Int Term Term

data Term = Var String



mercury :: Parser Bool
mercury = addn </> addn' </> multn </> multn' </> eqi </> addi </> multi </> inverse 
    
eqi:: Parser Bool
eqi = do
    string "eqi("
    n <- int'
    string ", "
    m <- int'
    return(n==m)
    
addi:: Parser Bool
addi = do
    string "addi("
    n <- int'
    string ", "
    o <- int'
    string ", "
    q <- int'
    return(n+o==q)

multi:: Parser Bool
multi = do
    string "multi("
    n <- int'
    string ", "
    o <- int'
    string ", "
    q <- int'
    return(n*o==q)

int':: Parser Int
int' = do
    string "int("
    n<-int
    char ','
    m<-int
    char ')'
    return(n - m)

addn':: Parser Bool
addn' = do
     string "addn("
     return False

addn:: Parser Bool
addn = do
     string "addn("
     a<-int
     char ','
     b<-int
     char ','
     c<-int
     return (a+b == c)

multn:: Parser Bool
multn = do
     string "multn("
     a<-int
     char ','
     b<-int
     char ','
     c<-int
     return (a*b == c)   

multn':: Parser Bool
multn' = do
     string "multn("
     return False


inverse:: Parser Bool
inverse = do
    string "inverse("
    n <- int'
    string ", "
    o <- int'
    return(n+o==0 && not(n-o==0))

https://www.pivotaltracker.com/n/projects/2472673

https://stackoverflow.com/questions/7423123/how-to-call-the-same-function-n-times
www.zvon.org/other/haskell/Outputprelude/replicate_f.html