################## Script for GAMs and LOESS ######################################

################## Import data
df_extended <- data.frame(read.csv("sicily_secondwave_covid.csv"))
df_extended$data <- as.Date(df_extended$data,  "%Y-%m-%d")
n <- dim(df_extended)[1]
df <- df_extended
df <- df[-c((n-13):n),]

df$data <- as.Date(df$data,  "%Y-%m-%d")
df_extended$data <- as.Date(df_extended$data,  "%Y-%m-%d")

############## GLM
library(mgcv)
library(ggplot2)

### Y ~ ricoverati_con_sintomi + nuovi_tamponi_pcr + color 
gam_mod1 <- gam(nuovi_positivi ~  ricoverati_con_sintomi + nuovi_tamponi_pcr + color  , data = df )
gam_mod1_pred <- predict(gam_mod1, type="response", se.fit = TRUE)
summary(gam_mod1)

ggplot() +
  geom_point(data = df, aes(x = data, y = nuovi_positivi, color="nuovi positivi", group = 1),col=c("#fc6b03","#cfcaca","#f2d729","#b3190b")[unclass(as.factor(df$color))])+ 
  geom_line(data = df, aes(x = data, y = predict(gam_mod1,type="response"), color = "model 4", group = 1)) +
  geom_ribbon(data=df, aes(x=data, ymin=gam_mod1_pred$fit - gam_mod1_pred$se.fit, ymax=gam_mod1_pred$fit + gam_mod1_pred$se.fit),alpha=0.2)+
  
  
  labs(x = "Time", y = "New positives") +
  scale_x_date(date_breaks = '5 days',
               date_labels = '%Y-%m-%d') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Predict

gam_mod_1_pred <- predict(gam_mod1, newdata=df_extended, type="response", se.fit = TRUE)

ggplot() +
  geom_point(data = df_extended, aes(x = data, y = nuovi_positivi, color="nuovi positivi", group = 1),col=c("#fc6b03","#cfcaca","#f2d729","#b3190b")[unclass(as.factor(df_extended$color))])+ 
  geom_line(data = df_extended, aes(x = data, y = predict(gam_mod1, newdata=df_extended, type="response"), color = "model 4", group = 1)) +
  geom_ribbon(data = df_extended, aes(x= data, ymin=-gam_mod_1_pred$se.fit + gam_mod_1_pred$fit, ymax=gam_mod_1_pred$se.fit + gam_mod_1_pred$fit), alpha=0.5)+
  
  
  labs(x = "Time", y = "New positives") +
  scale_x_date(date_breaks = '5 days',
               date_labels = '%Y-%m-%d') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### Y ~ (ricoverati_con_sintomi + nuovi_tamponi_pcr )* color + data

gam_mod2 <- gam(nuovi_positivi ~  (ricoverati_con_sintomi + nuovi_tamponi_pcr )* color  , data = df )
gam_mod2_pred <- predict(gam_mod2, type="response", se.fit = TRUE)
summary(gam_mod1)

ggplot() +
  geom_point(data = df, aes(x = data, y = nuovi_positivi, color="nuovi positivi", group = 1),col=c("#fc6b03","#cfcaca","#f2d729","#b3190b")[unclass(as.factor(df$color))])+ 
  geom_line(data = df, aes(x = data, y = predict(gam_mod2,type="response"), color = "model 4", group = 1)) +
  geom_ribbon(data=df, aes(x=data, ymin=gam_mod2_pred$fit - gam_mod2_pred$se.fit, ymax=gam_mod2_pred$fit + gam_mod2_pred$se.fit),alpha=0.2)+
  
  
  labs(x = "Time", y = "New positives") +
  scale_x_date(date_breaks = '5 days',
               date_labels = '%Y-%m-%d') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Predict

gam_mod_2_pred <- predict(gam_mod2, newdata=df_extended, type="response", se.fit = TRUE)

ggplot() +
  geom_point(data = df_extended, aes(x = data, y = nuovi_positivi, color="nuovi positivi", group = 1),col=c("#fc6b03","#cfcaca","#f2d729","#b3190b")[unclass(as.factor(df_extended$color))])+ 
  geom_line(data = df_extended, aes(x = data, y = predict(gam_mod2, newdata=df_extended, type="response"), color = "model 4", group = 1)) +
  geom_ribbon(data = df_extended, aes(x= data, ymin=-gam_mod_2_pred$se.fit + gam_mod_2_pred$fit, ymax=gam_mod_2_pred$se.fit + gam_mod_2_pred$fit), alpha=0.5)+
  
  
  labs(x = "Time", y = "New positives") +
  scale_x_date(date_breaks = '5 days',
               date_labels = '%Y-%m-%d') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##### Current data vs Prev data
glm_prev <- glm(nuovi_positivi ~ (ricoverati_con_sintomi_prev + nuovi_tamponi_pcr_prev)*color_prev, data = df, family = poisson) 
glm_current <- glm(nuovi_positivi ~ (ricoverati_con_sintomi + nuovi_tamponi_pcr)*color, data = df, family=poisson)
gam_prev <- gam(nuovi_positivi ~  (ricoverati_con_sintomi_prev + nuovi_tamponi_pcr_prev )* color_prev  , data = df )
gam_current <- gam(nuovi_positivi ~  (ricoverati_con_sintomi + nuovi_tamponi_pcr )* color  , data = df )
summary(glm_prev)
summary(glm_current)
summary(gam_prev)
summary(gam_current)
par(mfrow=c(2,2))
plot(glm_prev)
plot(glm_current)
gam.check(gam_prev)
gam.check(gam_current)

ggplot() +
  geom_point(data = df, aes(x = data, y = nuovi_positivi, color="nuovi positivi", group = 1),col=c("#fc6b03","#cfcaca","#f2d729","#b3190b")[unclass(as.factor(df$color))])+ 
  geom_line(data = df, aes(x = data, y = predict(glm_prev,type="response"), color = "model glm prev", group = 1)) +
  geom_line(data = df, aes(x = data, y = predict(glm_current,type="response"), color = "glm current", group = 1)) +
  geom_line(data = df, aes(x = data, y = predict(gam_prev,type="response"), color = "gam prev", group = 1)) +
  geom_line(data = df, aes(x = data, y = predict(gam_current,type="response"), color = "gam current", group = 1)) +
  
  
  labs(x = "Time", y = "New positives") +
  scale_x_date(date_breaks = '5 days',
               date_labels = '%Y-%m-%d') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
