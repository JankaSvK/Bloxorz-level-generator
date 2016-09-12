# Bloxorz-level-generator
*Jana Bátoryová*

Úlohou bolo vytvoriť automatický generátor levelov pre hru Bloxorz.

## Užívateľská dokumentácia ##
Program skompilujeme a spustíme pomocou nasledujúcich príkazov.
```
ghc zapoctak.hs
./zapoctak
```

Ďalej postupujeme podľa pokynov na obrazovke.

Potom program vypíše mapu s označením '#' kde sa políčka nachádzajú a '.' kde
nie. Dimenzia bloku určuje tvar "východu" (šírka, výška, hĺbka) a bod ĺavý
horný roh podstavy tohoto východu. Pre informáciu je uvedená aj výsledná
metrika mapy.

## Programátorská dokumentácia ##
Program pozostáva z dvoch logických celkov:
1. generovanie mapy
2. určenie metriky mapy

Generovanie mapy prebieha postupným náhodným otáčaním bloku. Po vykonaní
príslušného počtu krokov sa pre vytvorenú mapu spočíta jej metrika. Na jej
základe sa mapa zavrhne albo príjme. Pri zavrhnutí sa rekurzia o krok vráti a
počíta s inou možnosťou otočenia bloku.

Technicky sa generovanie vykonáva tak, že sa vytvoria všetky mapy danej hĺbky.
Pre nich sa spočíta metrika a zistí sa, či je vyhovujúce. Využíva sa lenivý
prístup Haskellu, ktorý zabezpečí, že sa v skutočnosti všetky mapy
nevygenerujú, ale skončí sa prvou vyhovujúcou.

Na určovanie metriky sa využíva miera nedeterminizmu kroku v každom políčku. U
každého dosiahnuteľného stavu sa spočíta počet možných krokov z tohoto stavu.
Odčíta sa jednotka a súčet štvorcov týchto medzivýpočtov je metrikou.

K tomuto bolo vytvorených mnoho pomocný funkcií, typov a dát.

## Vstupy ##
K programu sú v zložke ``` inputs ``` doložené tri ukážkové vstupy. Keďže sa
využíva náhodný seed, tak opätovným spúšťaním programu na rovnaké vstupy
získavame rôzne mapy.

## Možné vylepšenia ##
V programe sa dá vyhrať s inými metrikami a ich testovaním.

