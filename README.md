## Kompilacja

    $ make

## REPL

Uruchanianie

    $ ./repl

Ładowanie plików `:l nazwapliku`, np.:

    > :l Prelude.yl
    > :l examples.yl

REPL nie ładuje automatycznie pliku Prelude.yl z podstawowymi definicjami.
REPL nie wyświetla całej zawartości leniwie obliczanych list.
Do wyliczenia całej listy służy funkcja `show` (lub jej alias - operator prefiksowy `!`)


    > naturals
      [ 1 <not evaled expr>]
    > ! qsort [1 0 3 4]
      [ 0 1 3 4]
    > show $ qsort [1 0 3 4]
      [ 0 1 3 4]
    > qsort [1 0 3 4]
      [ 0 <not evaled expr>]

* autouzupełnianie pod klawiszem tab, historia ↑


## Run

Program można uruchomić poleceniem `./run plik1 [plik2, plik3...]`.
Moduł `Prelude.yl` zostanie dołączony automatycznie.
Zostanie uruchomiana funkcja `main` z argumentem którego wartością jest zawartość standardowego wejścia.
Na standardowe wyjście zostanie wypisana wartość zwrócona przez funkcję.

    $ echo "123456" | ./run examples.yl
    654321


## Typy:

* liczby całkowite
* listy (zapisywane jako: `[1 2 3]`)
* znaki (zapisywane jako: `'a'`)
* symbole (identyfikatory o wielkiej pierwszej literze np. `Symbol1`, `Symbol2`)


## Składnia:


Pattern matching. Np:

    sum a b = a + b;

    fact 0 = 1;
    fact n = n * (fact (n - 1));

    sum [] = 0;
    sum [x & xs] = x + (sum xs);
    f [a b c] = ...;
    g [first second & rest] = ...;

    insert [Empty] x = [Node x [Empty] [Empty]];
    insert [Node k l r] x = if (< x k) then [Node (insert l x) r] else [Node l (insert r x)];

    bind [Just x] f = f x;
    bind [Nothing] _ = [Nothing];

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
          (b a) + (g x);

Case:

    case x of
      1 -> 2;
      x -> x + 1;

Identyfikatory operatorów infiksowych to nazwa operatora poprzedzona znakiem '`'. Np.:

    `++ a b = a + a + b + b;
    (1 ++ 2) == 6

