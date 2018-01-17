# Illogical

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

## Grammar

- Qu $\to$ $\forall$ | $\exists$
- Op $\to$ $$ | $\lnot$ | $\land$ | $\lor$ …
- Con $\to$ string
- Var $\to$ string
- Func $\to$ string
- Pred $\to$ string

- Args $\to$ Term | Term, Args $\to$ List(Term)
- Term $\to$ Con | Var | Func(Args)
- Atom $\to$ Pred(Args)
- Form $\to$ Atom | Not Form | Form Op Form | Qu Var Form
- Literal $\to$ Atom | Not Atom
- Clause $\to$ Literal OR Literal

