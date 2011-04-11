## Typy:
* number
* lista (zapisywana jako "[1 2 3]")
* symbol (identyfikator o wielkiej pierwszej literze np. "Symbol1", "Symbol2")


## Składnia:

Pattern matching działający dla stałych i list. (Być może nie dla zagnieżdżonych list). Np:

    sum a b = a + b

    fact 0 = 1 ;
    fact n = n * (fact (n - 1)) ;

    sum [] = 0 ;
    sum [x & xs] = + x (sum xs) ;
    f [a b c] = ... ;
    g [first second & rest] = ... ;


    insert [Empty] x = [Node x [Empty] [Empty]] ;
    insert [Node k l r] x = if (< x k) then [Node (insert l x) r] else [Node l (insert r x)] ;

    bind [Just x] f = f x ;
    bind [Nothing] _ = [Nothing] ;

Lambda:

    \x -> x * x
    \a -> \b -> a + b

Let:

    f x = let 
          a = 1 ;
          b = \z -> z + x ;
          g 1 = 0 ;
          g x = x ;
       in 
          (b a) + (g x) 

Case:
