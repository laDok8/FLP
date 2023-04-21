## Turingův stroj
Vaším úkolem je vytvořit simulátor nedeterministického
Turingova stroje. Na vstupu váš program obdrží pravidla pro
Turingův stroj a vstupní obsah pásky. Výstupem bude
posloupnost konfigurací stroje.

Vnitřní stavy stroje jsou označeny velkými písmeny,
vstupní/páskovou abecedu tvoří malá písmena, prázdný symbol je mezera, počáteční stav je „S“,
koncový stav je „F“. Výpočet stroje začíná na začátku pásky a končí přechodem do koncového stavu
nebo abnormálním zastavením. Pravidla jsou zadána ve tvaru <stav> <symbol na pásce> <nový stav>
<nový symbol na pásce nebo „L“, „R“>. Jednotlivé části jsou odděleny mezerou, každé pravidlo bude
na samostatném řádku. Symboly L/R značí posun hlavy doleva/doprava. Na posledním řádku vstupu je
uveden vstupní obsah pásky (nekonečná posloupnost prázdných symbolů na konci pásky není
uvedena).

Jednotlivé konfigurace stroje vypisujte na samostatné řádky. Konfiguraci uvádějte v pořadí <obsah
pásky před hlavou><stav><symbol pod hlavou><zbytek pásky> (bez oddělovačů).

Zaměřte se na korektní vstupy a situace, kdy existuje sekvence přechodů do koncového stavu.
V takovém případě Váš program musí toto řešení najít. Implementační detaily v případě abnormálního
zastavení nebo cyklení jsou ponechány na autorovi.
Příklad vstupu a výstupu:
```
S a B a
B a B b
B b B R
B c B a
B c F c
B c B a % <- duplicit !
aaacaa

Saaacaa
Baaacaa
Bbaacaa
bBaacaa
bBbacaa
bbBacaa
bbBbcaa
bbbBcaa
bbbFcaa
```
