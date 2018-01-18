# Illogical (WIP)

Proste w użyciu narzędzia logiki pierwszego rzędu

## Features

- [x] Przekształcenia
	- [x] Wyrażenie operatora za pomocą AND, OR, NOT
	- [x] Prawo De Morgana
	- [x] Negacja kwantyfikatora
	- [x] Koniunkcyjna postać normalna (CNF)
	- [x] Przedrostkowa postać normalna (PNF)
	- [x] Przemianowanie zmiennych
- [x] Skolemizacja
	- [x] Wprowadzenie funkcji Skolema
- [x] Uzgadnianie
	- [x] Most general unifier (MGU)
- [ ] Metoda tablic semantycznych (MTS)
	- [ ] Spełnialność
	- [ ] Prawdziwość
	- [ ] Logiczna rownoważność
	- [ ] Logiczna konsekwencja
- [ ] Rezolucja
	- [ ] Spełnialność
	- [ ] Prawdziwość
- [ ] Interfejs wiersza poleceń
- [ ] Webowy interfejs

## How to run

```sh
sbt run
 ```

## How to test

```sh
sbt ~test
```

## Grammar

- Qu → ∀ | ∃
- Op → ∧ | ∨ | …
- Con → string
- Var → string
- Func → string
- Pred → string

- Args → Term | Term, Args → List(Term)
- Term → Con | Var | Func(Args)
- Atom → Pred(Args)
- Form → Atom | ¬Form | Form Op Form | Qu Var Form
- Literal → Atom | ¬Atom
- Clause → Literal OR Literal
