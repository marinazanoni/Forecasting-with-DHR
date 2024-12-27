##############################
##########INVESTIGATION#######
##############################
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

load(file= "data.RData")

#fix date
data$Data.Presentazione <- as.Date(data$Data.Presentazione)

#TAKE ONLY MUNICIPIo. drop all NA
municipi <- na.omit(data)

###############################
#plots solo municipi
giorno_counts <- table(municipi$Giorno)

# Crea l'istogramma
barplot(
  giorno_counts,
  main = "Day distribution",
  xlab = "Day",
  ylab = "Counts",
  col = "darkblue",
  border = "white"
)


mese_counts <- table(municipi$Mese)

# Crea l'istogramma
barplot(
  mese_counts,
  main = "Months distribution",
  xlab = "Mese",
  ylab = "Counts",
  col = "darkblue",
  border = "white"
)


#Distribuzione municipi + Area Tematica + Pericolo Immediato
counts <- table(municipi$Municipio, municipi$Area.Tematica, municipi$Pericolo.Immediato)

# Convertiamo i conteggi in un dataframe per il plotting
counts_df <- as.data.frame(as.table(counts))
names(counts_df) <- c("Municipio", "Area_Tematica", "Pericolo_Immediato", "Count")

# Plot
ggplot(counts_df, aes(x = Municipio, y = Count, fill = factor(Pericolo_Immediato))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Area_Tematica, scales = "free_y") +
  labs(x = "Municipio", y = "Count", fill = "Pericolo Immediato") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Distribuzione Municipio + Area Tematica + Pericolo Immediato")


###############################
########PRE-PROCESSING#########
###############################
#XBOOSTER
#SOLO AREA TEMATICA, MUNICIPIO, RECLAMO
municipi <- municipi %>%
  mutate(Anno = year(Data.Presentazione),
         Mese = month(Data.Presentazione),
         Giorno = day(Data.Presentazione))

agg_data <- municipi %>%
  group_by(Anno, Mese, Municipio, Area.Tematica) %>%
  summarise(Count = n()) %>%
  ungroup()

# Creare una serie temporale per ciascuna combinazione di municipio e area tematica
ts_data <- agg_data %>%
  unite(Date, Anno, Mese, sep = "-", remove = FALSE) %>%
  mutate(Date = as.Date(paste0(Date, "-01"))) %>%
  spread(key = Area.Tematica, value = Count, fill = 0)

dim(ts_data)

###############################
########TEMPORAL SERIES########
###############################







