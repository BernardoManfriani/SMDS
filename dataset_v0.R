##################
#### Project SMSD
##################

library(ggplot2)


#load data 
df_global <- data.frame(read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"))

#read data we are interested in
#then those that are relative to the following coupla of parameters:
#regione: Sicilia
#data:  1st October 2020 to 1st February 2021 
df_sicily_secondwave <- df_global[which(df_global$denominazione_regione == "Sicilia" & (df_global$data >= "2020-09-30T10:00:00" & df_global$data <= "2021-02-01T10:00:00")), ]
#df_em <-df_global[which(df_global$denominazione_regione == "Emilia-Romagna" & (df_global$data >= "2021-10-01T10:00:00" & df_global$data <= "2022-01-01T10:00:00")), ]

################
# clean dataset 
################

# remove unuseful columns
df <- df_sicily_secondwave[ , -which(names(df_sicily_secondwave) %in% c("stato", "codice_regione", "denominazione_regione", "lat", "long", "note"))]
df <- df[ , -c(17:24)]

# add column accounting for the number of new swabs carried out
for(x in 2:nrow(df)) {
  df$nuovi_tamponi[x] <- df$tamponi[x] - df$tamponi[x-1] 
}

# remove first row
df <- df[-1,]

#index reordering
row.names(df) <- NULL       

#add color
df$color <-NA
df$color[1:36]<-"bianco"
df$color[c(37:59, 89:91, 101:108)]<-rep("arancione",34)
df$color[c(60:84,99,100)]<-rep("giallo",27)
df$color[c(85:88,92:95,96,97,98, 109:123)]<-rep("rosso",25)


############################
### 1.Perform some explanatory analysis for your data, especially by use of graphical tools.
############################

df$data <- as.Date(df$data,  "%Y-%m-%d")
# see how the number of positives changes over time
ggplot() +
  geom_line(data = df, aes(x = data, y = nuovi_positivi, color="nuovi positivi", group = 1)) +
  geom_point(data = df, aes(x = data, y = nuovi_positivi, color="nuovi positivi", group = 1))+ 
  labs(x = "Time", y = "New positives") +
  scale_x_date(date_breaks = '5 days',
               date_labels = '%Y-%m-%d') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# see how the number of swabs carried out changes over time
ggplot() +
  geom_line(data = df, aes(x = data, y = tamponi, group = 1)) +
  geom_point(data = df, aes(x = data, y = tamponi, group = 1)) +
  labs(x = "Time", y = "Swabs") +
  scale_x_date(date_breaks = '5 days',
               date_labels = '%Y-%m-%d') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#########################
### 2.Describe the quality of the data and discuss whether and how a plausible statistical model could be posed.
#########################

# check for missing values in the data frame
na_count <- sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

