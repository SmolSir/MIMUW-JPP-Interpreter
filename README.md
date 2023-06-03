# MIMUW-JPP-Interpreter

## Spełnione wymagania
- spełnione są wszystkie wymagania z tabeli na 15 oraz 20 punktów,
- można korzystać z `break` oraz `continue` w pętli while,
- dostępnych jest wiele wariantów funkcji `print` do wypisywania (w zależności od typu, z automatycznie dodawanym na końcu znakiem nowej linii lub bez).

## Dalsze plany
- spróbować zaimplementować statyczne typowanie,
- wzbogacić / upiększyć obsługę błędów.

## Zapożyczenia
- przykładowa gramatyka Latte wzbogacona o:
  - instrukcje `break` oraz `continue`,
  - funkcje zwracające wartość,
  - przekazywanie argumentów do funkcji zarówno przez wartość, jak i przez referencję.

## Konflikty
Istnieje tylko jeden konflikt `shift/reduce`, dotyczący `if` oraz `if ... else`.

## Do zrobienia
- co jeśli nie ma `return` na końcu funkcji?
- :white_check_mark: leniwość
  - [`examples/good/05-03-laziness.txt`](https://github.com/SmolSir/MIMUW-JPP-Interpreter/blob/laziness-test/examples/good/05-03-laziness.txt)
- :white_check_mark: dlaczego istnieje `apply byRef` na `Expr` innym niż `Var`?
  - jakaś dziwna pozostałość, która nie dawała żadnych błędów i się prześlizgnęła
- :white_check_mark: porównania na stringach i boolean (co najmniej `==` i `/=`), dodawanie na stringach (`+`)
  - [`1b76c76`](https://github.com/SmolSir/MIMUW-JPP-Interpreter/pull/2/commits/1b76c768d849182966416fe447968a5865e1e89d)
- więcej przykładów / jakiś duży przykład
