#importare il file csv
#dati <- read.csv(StatisticheTiriSerieA.csv)

#visualizzare la tabella 
#head (dati)


#in questo caso ho aperto e caricato il file direttamente da (file>import data base)
#visualizzare tabella
head(StatisticheTiriSerieA)

#veridicare struttura dati
str(StatisticheTiriSerieA)

#**********************************************************************
#calcolare correlazione tra due variabili (tiri in porta e goal segnati)
correlazione <- cor(StatisticheTiriSerieA$`Tiri in porta`, StatisticheTiriSerieA$Reti)

#stampa a video del risultato
print(paste("Il coefficiente di correlazione tra i tiri in porta e le reti segnate è di:", round(correlazione, 3)))

#questo ci darà a video un valore tra -1 e +1 (che indica la forza e la direzione della relazione lineare tra le due variabili)
#Se vicino ad 1, allora vi è una forte correlazione positiva (quindi all'aumentare dei tiri in porta, tendono anche ad aumentare i goal segnati)
#se vicino a -1 (indica una forte correlazione negativa)
#se vicino allo zero, vi è una correlazione debole o nulla

#facciamo anche il plot, creando un garfico a dispersione
plot(StatisticheTiriSerieA$`Tiri in porta`, StatisticheTiriSerieA$Reti,
     xlab = "Numero di Tiri in Porta",
     ylab = "Numero di Gol Segnati",
     main = "Relazione tra Tiri in Porta e Gol Segnati",
     pch = 16, # Simbolo dei punti
     col = "blue")

# Aggiungi una linea di regressione lineare per visualizzare la tendenza
abline(lm(StatisticheTiriSerieA$`Tiri in porta` ~ StatisticheTiriSerieA$Reti), col = "red")


#**********************************************************************
#calcolare correlazione tra due variabili (reti e distanza tiri media)
mean(StatisticheTiriSerieA$distanza)  #calcolo della media della distanza dei tiri effettuati

correlazione2 <- cor(StatisticheTiriSerieA$distanza, StatisticheTiriSerieA$Reti)

#stampa a video del risultato
print(paste("Il coefficiente di correlazione tra i tiri in porta e le reti segnate è di:", round(correlazione2, 3)))

#facciamo anche il plot, creando un garfico a dispersione
plot(StatisticheTiriSerieA$distanza, StatisticheTiriSerieA$Reti,
     xlab = "Distanza media Tiri in Porta",
     ylab = "Numero di Gol Segnati",
     main = "Relazione tra Distanza Media Tiri e Gol Segnati",
     pch = 16, # Simbolo dei punti
     col = "blue")

# Aggiungi una linea di regressione lineare per visualizzare la tendenza
abline(lm(StatisticheTiriSerieA$distanza ~ StatisticheTiriSerieA$Reti), col = "red")

#**********************************************************************
#calcolare probabilità di fare goal in base alla distanza media dei tiri
# Definisci gli intervalli di distanza
intervalli_distanza <- c( 13, 15, 17, 19, Inf)
etichette_distanza <- c( "13-15", "15-17", "17-19", ">19")

# Crea una nuova colonna con gli intervalli di distanza
StatisticheTiriSerieA$intervallo_distanza <- cut(StatisticheTiriSerieA$distanza, breaks = intervalli_distanza, labels = etichette_distanza, right = FALSE)

# Aggrega i dati per intervallo di distanza
prob_segnare_per_distanza <- aggregate(Reti ~ intervallo_distanza, data = StatisticheTiriSerieA, FUN = function(x) sum(x) / length(x))

# Stampa i risultati
print(prob_segnare_per_distanza)

# Puoi anche visualizzare i risultati
library(ggplot2)
ggplot(prob_segnare_per_distanza, aes(x = intervallo_distanza, y = Reti)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Reti, 1), "%")), vjust = -0.3) +
  labs(title = "Probabilità di Segnare per Distanza", x = "Distanza (metri)", y = "Probabilità") +
  theme_minimal()

#come emerge dall'analisi, le squadre che tirano da una distanza più ravvicinata, sono soggette a segnare più goal


#**********************************************************************
#Calcolare correlazione tra Distanza e % goal per tiro in porta

correlazione_reti_efficacia <- cor(StatisticheTiriSerieA$distanza, StatisticheTiriSerieA$`goal per tiro in porta`)

# Stampa il risultato
print(paste("Correlazione tra Reti e Goal per tiro in porta:", round(correlazione_reti_efficacia, 3)))

#facciamo anche il plot, creando un garfico a dispersione
plot(StatisticheTiriSerieA$distanza, StatisticheTiriSerieA$`goal per tiro in porta`,
     xlab = "Distanza media Tiri in Porta",
     ylab = "Numero di Gol Segnati per tiri in porta",
     main = "Relazione tra Distanza Media Tiri e Gol Segnati per tiri in porta",
     pch = 16, # Simbolo dei punti
     col = "blue")

# Aggiungi una linea di regressione lineare per visualizzare la tendenza
abline(lm(StatisticheTiriSerieA$distanza ~ StatisticheTiriSerieA$`goal per tiro in porta`), col = "red")
