#### Load dataset 
library(data.table)

################################## Dati covid protezione civile #######################################################
df_global <- data.frame(read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"))

#select data about Sicily and the period to be considered
df_sicily_secondwave <- df_global[which(df_global$denominazione_regione == "Sicilia" & 
                                          (df_global$data >= "2020-09-16T10:00:00" & df_global$data <= "2021-02-15T10:00:00")), ]

# adjust number of swabs
df_sicily_secondwave[which(df_sicily_secondwave$data < "2021-01-15"),]$tamponi_test_molecolare <- 
  df_sicily_secondwave[which(df_sicily_secondwave$data < "2021-01-15"),]$tamponi

# remove unused columns
df_extended <- df_sicily_secondwave[ , -which(names(df_sicily_secondwave) %in% c("stato", "codice_regione", "denominazione_regione", 
                                                                                 "lat", "long", "note","tamponi_test_antigenico_rapido","codice_nuts_1","codice_nuts_2",
                                                                                 "casi_da_sospetto_diagnostico", "casi_da_screening",
                                                                                 "note_test",  "note_casi", "totale_positivi_test_molecolare", "totale_positivi_test_antigenico_rapido"))]

# put data in Date format
df_extended$data <- as.Date(df_extended$data,  "%Y-%m-%d")

# add color
df_extended$color <-NA
df_extended$color[which(df_extended$data >= "2020-09-16" & df_extended$data <= "2020-11-05")]<- "bianco"
df_extended$color[which((df_extended$data >= "2020-11-06" & df_extended$data <= "2020-11-28") | 
                          (df_extended$data >= "2020-12-28" & df_extended$data <= "2020-12-30") |
                          (df_extended$data >= "2021-01-09" & df_extended$data <= "2021-01-16") |
                          (df_extended$data >= "2021-02-01" & df_extended$data <= "2021-02-15"))] <- "arancione"
df_extended$color[which((df_extended$data >= "2020-11-29" & df_extended$data <= "2020-12-23") |
                          (df_extended$data >= "2021-01-07" & df_extended$data <= "2021-01-8"))] <- "giallo"
df_extended$color[which((df_extended$data >= "2020-12-24" & df_extended$data <= "2020-12-27") |
                          (df_extended$data >= "2020-12-31" & df_extended$data <= "2021-01-06") |
                            (df_extended$data >= "2021-01-17" & df_extended$data <= "2021-01-31"))] <- "rosso"

continuous_color <- function() {
  new_color <- NA
  previous = ""
  current = "bianco"
  new_color[1] = 0
  l <- df_extended$color
  count <- 0
  for (i in 2:length(l)) {
    if(l[i] == "bianco") {
      new_color[i] = new_color[i-1] - 0.1
      previous = "bianco"
      current = "bianco"
    }
    else if(l[i] == "rosso") {
      decrease = 1*0.1
      new_color[i] = new_color[i-1] + decrease
      if(current != "rosso") {
        previous = current 
        current = "rosso" 
      }
    }
    else if(l[i] == "giallo") {
      if(previous == "arancione" || previous  == "rosso"){
        decrease = -1*0.1
        new_color[i] = new_color[i-1] + decrease
      }  
      else {
        decrease = 1*0.1
        new_color[i] = new_color[i-1] + decrease
      }
      if(current != "giallo") {
        previous = current 
        current = "giallo" 
      }
    }
    else {
      if(previous == "rosso") {
        decrease = -1*0.1 
      }
      else {
        decrease = 1*0.1
      }
      new_color[i] =  new_color[i-1] + decrease
      if(current != "arancione") {
        previous = current 
        current = "arancione" 
      }
    }
  }
  l <- new_color
  return(l)
}

l <- continuous_color()
df_extended$new_color <- NA
df_extended$new_color <- l



# add columns accounting for the number of daily swabs, daily deaths,
# people daily discharged from the hospital and the number of ICU of one week before 
df_extended$nuovi_casi_testati <- NA
df_extended$nuovi_tamponi_pcr <- NA
df_extended$nuovi_decessi <- NA
df_extended$nuovi_dimessi <- NA
df_extended$terapia_intensiva_prev <- NA
df_extended$ricoverati_con_sintomi_prev <- NA
df_extended$nuovi_tamponi_pcr_prev <- NA
df_extended$nuovi_positivi_prev <- NA
df_extended$color_prev <- NA
df_extended$new_color_prev <- NA


# index reordering
row.names(df_extended) <- NULL 

for(x in 2:nrow(df_extended)) {
  df_extended$nuovi_casi_testati[x] <- df_extended$casi_testati[x] - df_extended$casi_testati[x-1]
  df_extended$nuovi_tamponi_pcr[x] <- df_extended$tamponi_test_molecolare[x] - df_extended$tamponi_test_molecolare[x-1] # the number of new swabs carried out] # the number of new swabs carried out
  df_extended$nuovi_decessi[x] <- df_extended$deceduti[x] - df_extended$deceduti[x-1] # daily deaths
  df_extended$nuovi_dimessi[x] <- df_extended$dimessi_guariti[x] - df_extended$dimessi_guariti[x-1] # variation in the number of people discharged from the hospital
}
week <- 7
for(x in (week+2):nrow(df_extended)) {
  df_extended$terapia_intensiva_prev[x] <- df_extended$terapia_intensiva[x-week]
  df_extended$ricoverati_con_sintomi_prev[x] <- df_extended$ricoverati_con_sintomi[x-week]
  df_extended$nuovi_tamponi_pcr_prev[x]  <- df_extended$nuovi_tamponi_pcr[x-week]
  df_extended$color_prev[x] <- df_extended$color[x-week]
  df_extended$new_color_prev[x] <- df_extended$new_color[x-week]
  df_extended$nuovi_positivi_prev[x] <- df_extended$nuovi_positivi[x-week]
}

# remove first rows 
df_extended <- df_extended[-c(1:15),]
row.names(df_extended) <- NULL 


################################## Google data #######################################################
#### Goolge data- Add data of google maps on the variation between the baseline
#set1 <-data.frame(read.csv("Global_Mobility_Report.csv"))
#set <- set1
#set$date <- as.Date(set$date, "%Y-%m-%d")
#set <- set[which(set$country_region_code == "IT"),]
#set <- set[which(set$sub_region_1 == "Sicily"),]
#set <-set[which(set$date >= "2020-09-16" ),]
#set <- set[which(set$date <= "2021-02-week"),]

#set <- set[c(1:152),]
#row.names(set) <- NULL

#fwrite(x=set,"google_data_sicily.csv")

# set <- data.frame(read.csv("google_data_sicily.csv"))
# 
# set$var_station_prev <- NA
# set$var_workplace_prev <- NA
# set$var_retail_prev <- NA
# for(x in (week+2):nrow(set)) {
#   set$var_station_prev[x] <- set$transit_stations_percent_change_from_baseline[x-week]
#   set$var_workplace_prev[x] <-set$workplaces_percent_change_from_baseline[x-week]
#   set$var_retail_prev[x] <-set$retail_and_recreation_percent_change_from_baseline[x-week]
# }
# 
# # remove first rows 
# set <- set[-c(1:15),]
# row.names(df_extended) <- NULL 
# 
# 
# df_extended$var_station <- set$transit_stations_percent_change_from_baseline
# df_extended$var_workplace <- set$retail_and_recreation_percent_change_from_baseline
# df_extended$var_retail <- set$workplaces_percent_change_from_baseline
# df_extended$var_station_prev <- set$var_station_prev
# df_extended$var_workplace_prev <- set$var_workplace_prev
# df_extended$var_retail_prev <- set$var_retail_prev

################ Save data ################
### save the dataframe in a csv file
fwrite(x=df_extended, file="sicily_secondwave_covid.csv")



# corr plot
# library("ggcorrplot")
# library("corrplot")
# cols <- c("ricoverati_con_sintomi","nuovi_decessi", "terapia_intensiva","nuovi_tamponi_pcr", "nuovi_positivi", "variation_transit_station", "variation_retail", "variation_workplace")
# M=cor(df_extended[,cols])
# colnames(M) <- c("A", "B", "C", "D", "E", "F", "G", "H")
# rownames(M) <- paste0(colnames(M), ". ", gsub("_", " ", cols))
# corrplot(M, method="number",tl.col="#a13c28", tl.srt = 360, tl.offset = 1, tl.cex=1.1)
# par(mfrow=c(1,1))

# # Regression on google data
# 
# par(mfrow = c(2,1))
# plot(nuovi_positivi ~ variation_transit_station, data = df_extended, pch =16, xlab = "variation train", ylab = "New positives",col=c("#fc6b03","#cfcaca","#f2d729","#b3190b")[unclass(as.factor(df_extended$color))])
# plot(nuovi_positivi ~ variation_retail, data = df_extended, pch =16, xlab = "variation retail", ylab = "New positives",col=c("#fc6b03","#cfcaca","#f2d729","#b3190b")[unclass(as.factor(df_extended$color))])
# plot(nuovi_positivi ~ variation_workplace, data = df_extended, pch =16, xlab = "variation workplace", ylab = "New positives",col=c("#fc6b03","#cfcaca","#f2d729","#b3190b")[unclass(as.factor(df_extended$color))])
# 
# 
# mod<-glm(nuovi_positivi ~ variation_workplace + variation_transit_station, data=df_extended, family=poisson)
# summary(mod)
# par(mfrow=c(2,2))
# plot(mod)
# library(ggplot2)
# ggplot(data = df_extended)+
#   geom_point(aes(x=data,y=nuovi_positivi))+
#   geom_line(aes(x=data, y=predict(mod, type="response")))
# 
# ## Regression with google data and other covariates
# mod<-glm(nuovi_positivi ~  variation_transit_station*(terapia_intensiva_prev + nuovi_tamponi_pcr_prev  + ricoverati_con_sintomi_prev) , data=df_extended, family=poisson)
# summary(mod)
# par(mfrow=c(2,2))
# plot(mod)
# library(ggplot2)
# ggplot(data = df_extended)+
#   geom_point(aes(x=data,y=nuovi_positivi))+
#   geom_line(aes(x=data, y=predict(mod, type="response")))





