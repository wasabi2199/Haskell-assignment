# Tento projekt obsahuje vypracovane zadanie z predmetu Funkcionalne programovanie FEI STUBA
# instrukcie k spusteniu
-stiahnite repozitar \n
-zadajte prikaz cabal run do terminalu (mali by sa stiahnut vsetky potrebne kniznice vdaka ich zadefinovaniu v fp-zad.cabal)
-po zbehnuti casti href parser zadajte slovo alebo slova, ktore sa maju v strankach vyhladat
# href parser - M. Polakova
Parser ktorý upravuje stránky pre pagerank. Tuto cast tvoria funkcie: appFile, mapPage a parsePages. parsePages funkcia vytvorí textový súbor s názvom "parserOutputPages.txt". V tomto súbore je na prvom mieste url stránky, ktorá bola parsovaná a ďaľšie sú url, ktoré sa na danej stránke nachádzajú (každá stránka je rozdelená čiarkou).
# html parser - S.D. Pekarekova
Parser ktory parsuje stranky od html tagov. Pouziva sa lib TagSoup. Pozostava z funkcii: getTag, getCloseTag, delTag, getParsedFile. getTag vybera iba obsahy tagov ktore zadame ako argument z celeho html obsahu, pouziva pri tom  getCloseTag, funkciu ktora hlada koniec daneho tagu. delTag odstranuje argumentom zadany tag z html obsahu. getParsedFile funkcia parsuje vsetky tagy a ich obsahy (texty) ktore nam ostali. prve tri spomenute funkcie pouzivame ak by sa v obsahu tagu html nachadzali nejake script tagy.
# reverzny index - F.M. Gajdos
Pouzivatel zada slova ktorych vyskyt chce najst v obsahu stranok. Pozostava z funkcii: findWordNew, getWebPages, getWebPageRanks, prepare_inv_index. findWordNew funckia hlada slovo vo vyparsovanej html stranke, ak sa tam nachadza vrati tuple s url stranky a jej pagerankom. getWebPages vracia list vyparsovanych stranok. getWebPageRanks vracia list pagerankov pre dane stranky (bol nacitavany zo suboru). prepare_inv_index prehladavaju sa vyparsovane html stranky a  odfiltruju sa tie stranky, v ktorych obsahu sa slovo nenachadzalo.
# main
Na zaciatku sa vola href parser (parsePages). Po zbehnuti tejto funkcie pouzivatel zada slova, ktore chce vyhladat. Vsetky slova sa vyhladaju v suboroch, vytvori sa prienik ich vyskytov a usporiadani prienik zostupne sa vypise do konzoly. 
