# Bloxorz-level-generator
*Jana Bátoryová*

* [Úloha](#úloha)
* [Užívateľská dokumentácia](#užívateľská-dokumentácia)
* [Programátorská dokumentácia](#programátorská-dokumentácia)
* [Vstupy](#vstupy)
* [Možné vylepšenia](#možné-vylepšenia)

## Úloha

Hra Bloxorz je známa online hra, v ktorej úlohou je presunúť blok do cieľa.
Blok má tvar kvádru a presúvať je ho možné preklápaním cez jeho štyri hrany
(štyri smerové šípky). Cieľ pozostáva z diery, do ktorej musí blok vpadnúť
(nestačí dieru prekryť). Hráč si musí dať pozor, aby počas presunu k cieľu
žiadnou svojou časťou bloku nestál na vzduchu. V takom prípade blok spadne a
hráč prehral.

[Príklad jedného levelu](http://www.albinoblacksheep.com/flash/960/bloxorz.jpg)

Úlohou bolo vytvoriť automatický generátor levelov s nastaviteľnou
obtiažnosťou.

Tento zápočtový program zahrňuje generovanie mapy pre ľubovoľný kváder.

## Užívateľská dokumentácia ##
Program skompilujeme a spustíme pomocou nasledujúcich príkazov.
```
ghc zapoctak.hs
./zapoctak
```

Interaktívny program nás sprevádza nastavením parametrov pre level, ktorý
vygeneruje.

```
Zadajte rozmery bloku -- šírka výška hĺbka --
Zadajte veľkosť mapy -- šírka výška --
Zadajte počiatočné súradnice -- x y --
Zadajte minimálnu obtiažnosť mapy
Zadajte hĺbku generovanéj mapy
```
Rozmery bloku sa zadávajú na počet políčok. Veľkosť mapy určuje obdĺžnik, v
ktorom generátor smie generovať mapu. Užívateľ taktiež určí miesto štartu.

Do programu sa zadáva minimálna obtiažnosť mapy vzhľadom na algoritmus, ktorý
využíva - podrobnejší popis viď [Programátoská
									 dokumentácia](#programátorská-dokumentácia). Program zaručí, že vygenerovaná mapa bude mať aspoň takú obtiažnosť.
Obtiažnosť vygenerovanej mapy je zahrnutá vo výpise.  
Vhodné hodnoty: 50 - 350  
*Pri jednotlivých rozmeroch mapy a blokov je možné hodnoty mierne navýšiť, či
znížiť.*

Hĺbka generovanej mapy zahrňuje taktiež údaje pre algoritmus, viď
[Programátorská dokumentácia](#programátorská-dokumentácia).
Tento údaj určuje to, ako dlho sa mapa generuje.  
Vhodná hodnota: 30  
*Jej zmena môže slúžiť k experimentovaniu*


Po prijatí parametrov pre program sa generátor spustí. Následne vypíše mapu 
s označením '#' kde sa políčka nachádzajú a '.' kde nie. Dimenzia bloku 
určuje tvar "východu" (šírka, výška, hĺbka) a bod ĺavý
horný roh podstavy tohoto východu. Pre informáciu je uvedená aj výsledná
metrika mapy.

## Programátorská dokumentácia ##
Program pozostáva z dvoch logických celkov:  
1. generovanie mapy
2. určenie metriky mapy

Algoritmus vychádza na začiatku z prázdnej mapy. Podľa počiatočných súradníc
umestni pomyselný blok. Náhodne týmto blokom otáča do všetkých smerov, pričom
pri každom otočení označí políčka pod ním za "pevné". Hĺbka generovania zadaná
pri vstupe značí počet týchto otočení bloku. 

Následne sa pre mapu spočíta jej metrika. Na jej
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

