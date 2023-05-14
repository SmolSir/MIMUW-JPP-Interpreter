# MIMUW-JPP-Interpreter

## Co udało się zrobić
- spełnione są wszystkie wymagania z tabeli na 15 oraz 20 punktów,
- można korzystać z `break` oraz `continue` w pętli while,
- dostępnych jest wiele wariantów funkcji `print` do wypisywania (w zależności od typu, z automatycznie dodawanym na końcu znakiem nowej linii lub bez).

## Jakie są dalsze plany
- spróbować zaimplementować statyczne typowanie,
- wzbogacić / upiększyć obsługę błędów.

## Zapożyczenia
- przykładowa gramatyka Latte wzbogacona o:
  - instrukcje `break` oraz `continue`,
  - funkcje zwracające wartość,
  - przekazywanie argumentów do funkcji zarówno przez wartość, jak i przez referencję.

## Konflikty
Istnieje tylko jeden konflikt `shift/reduce`, dotyczący `if` oraz `if ... else`.
