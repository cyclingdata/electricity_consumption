# chargement des packages utiles

library(tidyverse) # manip donnees
library(zoo) # idem
library(lubridate) # traitement des dates
library(ranger) # random forest
library(broom) # recuperer facilement les infos des modeles
library(caret) # pour jouer sur les parametres des modeles
library(e1071)
library(imputeTS) # pour gestion des valeurs manquantes

T1 <- Sys.time()

# lecture des fichiers publicDataset et submission
# calculer des variables qui nous seront utiles par la suite
dataset <- read_csv("publicDataset.csv") %>% mutate(SiteId=as.factor(SiteId))
dataset <- dataset %>% mutate(wday =weekdays(Time),hour = hour(Time),mois=month(Time),date=date(Time), jour=day(Time),hour_comp = hour(Time) + minutes(Time)/60,saison =
                                case_when(mois %in% 1:3 ~"hiver", mois %in% 4:6 ~"printemps", mois %in% 7:9 ~"été", TRUE ~"automne"))
dataset <- dataset %>% group_by(SiteId) %>% mutate(rank=row_number()) %>% ungroup()
dataset1 <- dataset %>% filter(!is.na(Consumption)) %>% group_by(SiteId) %>% mutate(rank2=row_number()) %>% ungroup()
submission <- read_csv("submissionFile.csv")%>% mutate(SiteId=as.factor(SiteId))
submission$rang <- 1:nrow(submission)

ggplot(dataset1,aes(x=Time,y=Temperature)) + geom_line(size=0.5) + facet_wrap(~SiteId) + xlab("Time") + ylab("Temperature")

moy_jour <- dataset1 %>% group_by(SiteId,date) %>% summarize(temp=mean(Temperature,na.rm=TRUE))

ggplot(moy_jour,aes(x=date,y=temp)) + geom_line(size=0.3) + facet_wrap(~SiteId) + xlab("Date")+ ylab("Température (moyenne journalière)")

moy_heure <- dataset1 %>% group_by(SiteId,hour) %>% summarize(temp=mean(Temperature,na.rm=TRUE))

ggplot(moy_heure,aes(x=hour,y=temp)) + geom_line(size=0.3) + facet_wrap(~SiteId) + xlab("Heure")+ ylab("Température moyenne")


# garder les dernieres valeurs en test (autant que de pred à faire)
nb_valeurs_site <- dataset1 %>% group_by(SiteId) %>% count() %>% rename(nb_obs_site=n)
nb_valeurs_test_site <- submission %>% group_by(SiteId) %>% count() %>% rename(nb_test_site=n)

dataset1 <- dataset1 %>% left_join(nb_valeurs_site) %>% left_join(nb_valeurs_test_site) %>% mutate(type_obs = ifelse(rank2 <= nb_obs_site-nb_test_site,"train","test"))

dataset <- dataset %>% left_join(dataset1 %>% select(SiteId,rank,type_obs))
# 1. prediction de la temperature pour un site et un jour donne à 13h

# methode tres simple : moyenne mobile 7 j des temperatures observees pour le site
extract13 <- dataset %>% filter(hour==13,type_obs=="train") %>% group_by(SiteId,date,jour,mois) %>% summarize(temp_13h = median(Temperature,na.rm=TRUE)) %>% ungroup()
dates_min_max <- extract13 %>% group_by(SiteId) %>% summarize(min_date = min(date),max_date=max(date)) %>% mutate(nb_jours=max_date - min_date +1)

liste_jours <- unique(extract13 %>% ungroup() %>% select(jour,mois)) %>% mutate(combo=paste0(jour,"/",mois)) %>% select(combo)
liste_jours_site <- expand.grid(combo = liste_jours$combo,SiteId= factor(1:13)) %>% mutate(jour=as.numeric(str_split_fixed(combo,"/",2)[,1]),
                                                                                           mois=as.numeric(str_split_fixed(combo,"/",2)[,2])) %>% select(SiteId,jour,mois)

extract13 %>% group_by(SiteId,jour, mois) %>% count() %>% group_by(SiteId) %>% count()
extract13 <- extract13 %>%
  group_by(SiteId) %>%
  arrange(SiteId,date) %>%
  mutate(temp.7j = rollmedian(x = temp_13h, 7, align = "center",fill=na.interpolation(temp_13h,option = "linear")),
         temp.1j = rollmedian(x = temp_13h, 1, align = "center",fill=NA),
         temp.15j =rollmedian(x = temp_13h, 15, align = "center",fill=NA),
         temp.30j =rollmedian(x = temp_13h, 30, align = "center",fill=NA)) %>% mutate(jour=day(date),mois=month(date))

pred_temp_jour <- extract13 %>% group_by(SiteId,jour,mois) %>% summarize(temp_pred_13h = median(temp.7j,na.rm=TRUE),
                                                                         temp_pred_13h_15 = median(temp.15j,na.rm=TRUE),
                                                                         temp_pred_13h_1 = median(temp.1j,na.rm=TRUE)) %>% ungroup()

pred_temp_jour <- pred_temp_jour %>% full_join(liste_jours_site)

# gerer les jours manquants
pred_temp_jour <- pred_temp_jour %>% arrange(SiteId,mois,jour) %>% mutate(temp_pred_13h= na.interpolation(temp_pred_13h,option="linear"))

extract13 <- extract13 %>% left_join(pred_temp_jour)


plot13 <- extract13 %>% select(SiteId,date,temp_13h,temp_pred_13h) %>% rename( temperature_reelle = temp_13h,prediction=temp_pred_13h) %>%
  gather(key=type_temp,value=temp,-date,-SiteId) %>% mutate(type_temp=factor(type_temp,levels=c("temperature_reelle","prediction")))


ggplot(plot13,aes(x=date,y=temp,group=type_temp,color=type_temp)) + geom_line(size=0.5) + facet_wrap(~SiteId)
# 2. pour chaque heure de la journee et chaque site, faire un modele lineaire qui predit la temp en fonction de la temp à 13h
# ainsi a partir de la pred à 13h on pourra faire une prediction de la pred à n'importe quelle heure de la journee

# calculer la moyenne temp dans une mm tranche horaire

dataset_heure <- dataset %>% filter(type_obs=="train") %>% group_by(SiteId,date,mois,hour) %>% summarize(Temperature=median(Temperature),Consumption=median(Consumption))

temp_day <- dataset_heure %>% ungroup() %>% mutate(hour=paste0("h",hour)) %>% select(date,SiteId,Temperature,hour) %>% spread(hour,Temperature)


# calcul des modeles
cpt <- 1

for (i in setdiff(0:23,13)){
  fmle <- as.formula(paste0("h",i,"~ h13"))
  for (j in 1:13) {
    modele <- lm(fmle,data=temp_day %>% filter(SiteId==j))
    
    if (cpt==1){
      res <- glance(modele)
      res$SiteId <- j
      res$hour <- i
      res$slope <- as.numeric(modele$coefficients[2])
      res$intercept <- as.numeric(modele$coefficients[1])
    } else {
      res_tmp <- glance(modele)
      res_tmp$SiteId <- j
      res_tmp$hour <- i
      res_tmp$slope <- as.numeric(modele$coefficients[2])
      res_tmp$intercept <- as.numeric(modele$coefficients[1])
      res <- rbind(res,res_tmp)
    }
    cpt <- cpt + 1
  }
}


res_pred <- res %>% select(SiteId,hour,slope,intercept)
res_pred <- res_pred %>% rbind(data_frame(SiteId=1:13,hour=13,slope=1,intercept=0)) %>% mutate(SiteId=as.factor(SiteId))

# application sur les valeurs test et train

dataset1 <- dataset1 %>% left_join(res_pred) %>% left_join(pred_temp_jour)
dataset1 <- dataset1 %>% mutate(temp_complet =intercept + temp_pred_13h *slope )
ggplot(dataset1 %>% filter(type_obs=="test",hour>=7,hour<=19),aes(x=Temperature,y=temp_complet)) + geom_point(alpha=0.1,color='cyan') + geom_abline() + facet_wrap(~SiteId,scales="free") +
  xlab("Temperature réelle") + ylab("Température prévue")



# 3. Construire un modele randomforest qui predit la conso en fonction de la temperature, l heure de la journee, le jour de la semaine, le site et le mois

# pour certains sites la consommation a radicalement change : dans ce cas enlever les anciennes conso qui ne semblent plus correspondre a la realite
dataset2 <- dataset1 %>% filter(SiteId != 1 | date >=ymd("2015-01-01"),SiteId != 12 | date >= ymd("2016-07-01"),SiteId != 11 |
                                  date >= ymd("2016-12-01"),type_obs=="train") %>% filter(!is.na(Consumption))


dataset2 <- dataset2 %>% mutate(Temperature= ifelse(is.na(Temperature),temp_complet,Temperature))


model_rf <- ranger(Consumption ~ Temperature + hour + SiteId + wday+ mois,
                   data = dataset2, # data
                   mtry = 3,
                   num.trees=500)
gc()

# application sur le test
dataset1$Temperature_sauv <- ifelse(is.na(dataset1$Temperature),dataset1$temp_complet,dataset1$Temperature)
dataset1$Temperature <- dataset1$temp_complet
dataset1$predConso <- predict(model_rf,dataset1)$predictions
ggplot(dataset1 %>% filter(type_obs=="test"),aes(x=Consumption,y=predConso)) + geom_point(alpha=0.2) + geom_abline() + facet_wrap(~SiteId,scales="free")


# calcul de l erreur
moy_conso_site <- dataset1 %>% filter(type_obs=="test") %>% group_by(SiteId) %>% summarize(mu = mean(Consumption)) %>% ungroup()
N <- 13


test <- dataset1 %>% filter(type_obs=="test") %>% group_by(SiteId) %>% arrange(Time) %>% mutate(t = row_number()) %>% ungroup() %>%
  left_join(moy_conso_site) %>% left_join(nb_valeurs_test_site)

test <- test %>% mutate(erreur = (3*nb_test_site - 2* t+1)/(2*nb_test_site^2) * (Consumption-predConso)^2)

erreur <- test %>% group_by(SiteId) %>% summarize(erreur = sqrt(sum(erreur))) %>% left_join(moy_conso_site) %>% mutate(erreur=erreur/mu) %>%
  ungroup() %>% summarize(erreur=sum(erreur)/N)
erreur

# 4. application aux valeurs à prédire

submission <- submission %>% mutate(wday =weekdays(Time),hour = hour(Time),mois=month(Time),date=date(Time),jour=day(Time),hour_comp = hour(Time) + minutes(Time)/60) %>% left_join(res_pred)
submission <- submission %>% left_join(pred_temp_jour)

submission <- submission %>% mutate(Temperature =intercept + temp_pred_13h *slope )
submission$Consumption <- predict(model_rf,submission)$predictions

out <- submission %>% mutate(Time2=as.character(Time)) %>% select(SiteId,Consumption,Time2) %>% rename(Time=Time2,Prediction=Consumption)

write_csv(out,"Marmousez_France_1.csv")

T2 <- Sys.time()

difftime(T2,T1)