pacman::p_load(data.table, lubridate, magrittr, questionr, tidyverse, ggplot2)

# Empilement des bases
annees <- c(2018:2020, "20212022")
deces <- lapply(annees, function(an) fread(paste0("Data/2022-02-25_detail_2022/DC_", an, "_det.csv"))) %>%
  rbindlist(fill =  T)

# Date de décès
deces[, date_dec := ymd(paste(ADEC, MDEC, JDEC, sep = "-"))]

# Date de naissance (janvier quand mois inconnu, 1er quand jour inconnu)
deces[, date_nais := ymd(paste(ANAIS, 
                               fifelse(is.na(MNAIS) | MNAIS == 0, 1, MNAIS),
                               fifelse(is.na(JNAIS) | JNAIS == 0, 1, JNAIS), 
                               sep = "-"))]

# Âge au moment du décès
deces[, age := interval(date_nais, date_dec) %/% years(1)]


# Dataframe ?volution de la mortalit? sur diff?rentes p?riode
evolution_mortalite <- data.frame(annee = c(2018, 2019, 2020), premier_conf_M = NA, deuxieme_conf_M = NA, 
                                  premier_conf_F = NA, deuxieme_conf_F = NA)



# Nombre de mort hommes entre 18 et 30 ans, en 2018, 2019 et 2020, entre les dates du premier confinnement, 
# seulement le premier mois pour calculer un taux de mortalit? par mois comparable.
evolution_mortalite[1,"premier_conf_M"] <- count(deces[deces$date_dec >= "2018-03-17" & deces$date_dec <= "2018-04-17" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "M",])

evolution_mortalite[2,"premier_conf_M"] <- count(deces[deces$date_dec >= "2019-03-17" & deces$date_dec <= "2019-04-17" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "M",])

evolution_mortalite[3,"premier_conf_M"] <- count(deces[deces$date_dec >= "2020-03-17" & deces$date_dec <= "2020-04-17" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "M",])

# Deuxi?me confinement

evolution_mortalite[1,"deuxieme_conf_M"] <- count(deces[deces$date_dec >= "2018-10-30" & deces$date_dec <= "2018-11-30" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "M",])

evolution_mortalite[2,"deuxieme_conf_M"] <- count(deces[deces$date_dec >= "2019-10-30" & deces$date_dec <= "2019-11-30" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "M",])

evolution_mortalite[3,"deuxieme_conf_M"] <- count(deces[deces$date_dec >= "2020-10-30" & deces$date_dec <= "2020-11-30" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "M",])

# Pour les femmes
# premier confinement
evolution_mortalite[1,"premier_conf_F"] <- count(deces[deces$date_dec >= "2018-03-17" & deces$date_dec <= "2018-04-17" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "F",])

evolution_mortalite[2,"premier_conf_F"] <- count(deces[deces$date_dec >= "2019-03-17" & deces$date_dec <= "2019-04-17" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "F",])

evolution_mortalite[3,"premier_conf_F"] <- count(deces[deces$date_dec >= "2020-03-17" & deces$date_dec <= "2020-04-17" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "F",])

# Deuxi?me confinement

evolution_mortalite[1,"deuxieme_conf_F"] <- count(deces[deces$date_dec >= "2018-10-30" & deces$date_dec <= "2018-11-30" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "F",])

evolution_mortalite[2,"deuxieme_conf_F"] <- count(deces[deces$date_dec >= "2019-10-30" & deces$date_dec <= "2019-11-30" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "F",])

evolution_mortalite[3,"deuxieme_conf_F"] <- count(deces[deces$date_dec >= "2020-10-30" & deces$date_dec <= "2020-11-30" 
            & deces$age >= 18 & deces$age <= 30 & deces$SEXE == "F",])



evolution_mortalite$population_M_18_30 <- NA
evolution_mortalite$population_F_18_30 <- NA

#population au premier janvier 2020
#Hommes  entre 18 et 30 =	4 822 540
evolution_mortalite$population_M_18_30[evolution_mortalite$annee == 2020] <- 4822540
#Femmes entre 18 et 30 =	4 776 592
evolution_mortalite$population_F_18_30[evolution_mortalite$annee == 2020] <- 4776592

#Population au premier janvier 2019
# Hommes 18 - 30 4 834 216
evolution_mortalite$population_M_18_30[evolution_mortalite$annee == 2019] <- 4834216
# Femmes 18 - 30 4 796 563
evolution_mortalite$population_F_18_30[evolution_mortalite$annee == 2019] <- 4796563

#Population au premier janvier 2018
#hommes 18 - 30 4 858 295
evolution_mortalite$population_M_18_30[evolution_mortalite$annee == 2018] <- 4858295
#femmes 18 - 30	4 824 962
evolution_mortalite$population_F_18_30[evolution_mortalite$annee == 2018] <- 4824962


#Taux de mortalit? pour 100 000 pour le mois du premier confinement chaque ann?es
#Homme
evolution_mortalite$taux_mortalite_premier_conf_M <- (evolution_mortalite$premier_conf_M/evolution_mortalite$population_M_18_30) * 100000
# Femme
evolution_mortalite$taux_mortalite_premier_conf_F <- (evolution_mortalite$premier_conf_F/evolution_mortalite$population_F_18_30) * 100000

#Taux de mortalit? pour 100 000 pour le mois du deuxieme confinement chaque ann?es
#Homme
evolution_mortalite$taux_mortalite_deuxieme_conf_M <- (evolution_mortalite$deuxieme_conf_M/evolution_mortalite$population_M_18_30) * 100000
# Femme
evolution_mortalite$taux_mortalite_deuxieme_conf_F <- (evolution_mortalite$deuxieme_conf_F/evolution_mortalite$population_F_18_30) * 100000


taux_plot <- ggplot(evolution_mortalite, aes(x = annee)) +  
  geom_line(aes(y = taux_mortalite_premier_conf_M), color = "blue") +
  geom_line(aes(y = taux_mortalite_premier_conf_F), color = "red") +
  geom_line(aes(y = taux_mortalite_deuxieme_conf_M), color = "blue", linetype = "dashed") +
  geom_line(aes(y = taux_mortalite_deuxieme_conf_F), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = c(2018, 2019, 2020))

taux_plot


# Exportation de la base complète
# fwrite(deces, "Data/deces_2018-2022.csv")


