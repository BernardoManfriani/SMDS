#### Load dataset 
library(data.table)

df_global <- data.frame(read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"))

#select data about Sicily and the period to be considered
df_sicily_secondwave <- df_global[which(df_global$denominazione_regione == "Sicilia" & 
                                          (df_global$data >= "2020-09-17T10:00:00" & df_global$data <= "2021-02-15T10:00:00")), ]

# adjust number of swabs
df_sicily_secondwave[which(df_sicily_secondwave$data < "2021-01-15"),]$tamponi_test_molecolare <- 
  df_sicily_secondwave[which(df_sicily_secondwave$data < "2021-01-15"),]$tamponi

# remove unused columns
df_extended <- df_sicily_secondwave[ , -which(names(df_sicily_secondwave) %in% c("stato", "codice_regione", "denominazione_regione", "lat", "long", "note"))]
df_extended <- df_extended[ , -c(22:24)]
df_extended <- df_extended[ , -c(16:20)]


# put data in Date format
df_extended$data <- as.Date(df_extended$data,  "%Y-%m-%d")

# add color
df_extended$color <-NA
df_extended$color[1:50]<- "bianco"
df_extended$color[c(51:74, 103:105, 115:122, 138:151)] <- "arancione"
df_extended$color[c(74:98,113,114)]<- "giallo"
df_extended$color[c(99:102,106:112, 123:137)]<- "rosso"

# add columns accounting for the number of daily swabs, daily deaths,
# people daily discharged from the hospital and the number of ICU of one week before 
df_extended$nuovi_tamponi_pcr <- NA
df_extended$nuovi_decessi <- NA
df_extended$nuovi_dimessi <- NA
df_extended$terapia_intensiva_prev <- NA
df_extended$ricoverati_con_sintomi_prev <- NA
df_extended$nuovi_tamponi_pcr_prev <- NA
df_extended$nuovi_positivi_prev <- NA
df_extended$color_prev <- NA

# index reordering
row.names(df_extended) <- NULL 

for(x in 2:nrow(df_extended)) {
  df_extended$nuovi_tamponi_pcr[x] <- df_extended$tamponi_test_molecolare[x] - df_extended$tamponi_test_molecolare[x-1] # the number of new swabs carried out] # the number of new swabs carried out
  df_extended$nuovi_decessi[x] <- df_extended$deceduti[x] - df_extended$deceduti[x-1] # daily deaths
  df_extended$nuovi_dimessi[x] <- df_extended$dimessi_guariti[x] - df_extended$dimessi_guariti[x-1] # variation in the number of people discharged from the hospital
}

for(x in 16:nrow(df_extended)) {
  df_extended$terapia_intensiva_prev[x] <- df_extended$terapia_intensiva[x-14]
  df_extended$ricoverati_con_sintomi_prev[x] <- df_extended$ricoverati_con_sintomi[x-14]
  df_extended$nuovi_tamponi_pcr_prev[x]  <- df_extended$nuovi_tamponi_pcr[x-14]
  df_extended$color_prev[x] <- df_extended$color[x-14]
  df_extended$nuovi_positivi_prev[x] <- df_extended$nuovi_positivi[x-14]
}

# remove first rows 
df_extended <- df_extended[-c(1:15),]
row.names(df_extended) <- NULL 


df <- df_extended[1:122,]
df$data <- as.Date(df$data,  "%Y-%m-%d")

fwrite(x=df, file="sicily_secondwave_covid.csv")
