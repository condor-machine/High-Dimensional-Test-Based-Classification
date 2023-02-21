# High-Dimensional-Test-Based-Classification
High dimensional test-based classification: An extension of test-based classification to high dimensional data

Questa repository contiene tutto il materiale che sta attorno al modello porposto nella mia tesi di laurea magistrale:

## tesi e presentazione
In tesi e presentazione sono contenuti due file pdf. Uno relativo alla tesi di laurea in sé, mentre l'altro relativo alla presentazione della tesi sotto forma di slides

## funzioni per classificazioni
In funzioni per classificazioni ci sono 3 files:
- Uno relativo all'implementazione da zero di un modello di classificazione chiamato Instance Based Classification (ibc.R), proposto per la prima volta in https://ieeexplore.ieee.org/abstract/document/9333560 . In particolare in ibc.R vi è una modifica di tale modello per la selezione dell'iperparametro relativo al numero di nearest neighbour in maniera automatica tramite CV.
- Uno relativo all'implementazione da zero di un modello di classificazione chiamato Test Based Classification (htbc.R), proposto per la prima volta in https://www.sciencedirect.com/science/article/abs/pii/S0167715207000909 . Inoltre, dato che questo modello è applicabile solo in contesti con p<=n, è stata aggiunta la possibilità di poterlo applicare in contesti con p>n, modello sviluppato da zero nella mia tesi presente nella sezione "tesi e presentazione" e chiamato HDTBC (High Dimensional Test Based Classification)
- Uno relativo ad una funzione per effettuare classificazioni binarie con un modello a scelta e con una convalida incrociata annidata, per poter stimare l'accuratezza e scegliere gli iperparametri in maniera automatizzata.

## esempi_pratici
In esempi_pratici ci sono 3 cartelle:
- Una che contiene i dataset analizzati tramite i vari modelli di classificazione proposti in tesi, sia modelli stato dell'arte che modelli implementati da zero (IBC, TBC e HDTBC)
- Una che contiene il codice per pulire e preparare i dataset sopracitati, sui quali (sempre all'interno di questa cartella) vengono misurate le performance predittive dei vari modelli in questione
- Una che riassume i risultati dei modelli e fornisce i codici per plottare i vari risultati, oltre che ai grafici veri e propri

## simulazioni
In questa cartella sono presenti i codici per generare varie simulazioni per confrontare in vari contesti (al variare del rapporto fra numero di predittori e numero di osservazioni) le performance di HDTBC ed SDA; quest'ultima è un'estensione dell'LDA a contesti con p>n che si basa sullo stesso stimatore della matrice di varianza-covrianza dell'HDTBC e con cui condivide molte proprietà. Oltre ai codici per generare le simulazioni ci sono anche i risultati, riportati sottoforma di grafici.
