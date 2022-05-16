pacman::p_load(data.table, lubridate, magrittr, foreign)
theme_set(theme_minimal())
year_colours <- viridis::magma(2, begin = 0.3, end = 0.7)

# 2018-2020 ----
deces <- fread("Data/deces_2018-2022.csv")[ADEC %in% 2018:2020 & 
                                             !(DEPDEC %in% 971:978) &
                                             age >= 18 & age < 30]

# Aggrégation par jour
deces_jours_1820 <- merge(
  data.table(date_dec = seq(ymd("2018-01-01"), ymd("2020-12-31"), by = "days")),
  deces[, .(ndec = as.numeric(.N)), 
        keyby = .(date_dec)],
  by = "date_dec", all.x = T)
deces_jours_1820[, `:=`(ADEC = as.integer(format(date_dec, "%Y")),
                        MDEC = as.integer(format(date_dec, "%m")),
                        md_dec = format(date_dec, "%m-%d"))]


# Moyenne 2018-2019
deces_jours_1820 <- rbind(
  deces_jours_1820,
  deces_jours_1820[ADEC %in% 2018:2019, 
                   .(ndec = mean(ndec), 
                     ADEC = "2018-2019"), 
                   keyby = md_dec],
  fill = T)



# Ajout du nombre de décès par sexe
deces_jours_1820[deces[, .(ndec = as.numeric(.N)), 
                       keyby = .(date_dec, SEXE)] %>%
                   dcast(date_dec ~ SEXE, value.var = "ndec"),
                 `:=`(ndec_fem = i.F, ndec_hom = i.M), 
                 on = "date_dec"]

# Moyenne 2018-2019  
deces_jours_1820[deces_jours_1820[ADEC %in% c("2018", "2019"), 
                                  .(ndec_fem = mean(ndec_fem), 
                                    ndec_hom = mean(ndec_hom),
                                    ADEC = "2018-2019"), keyby = "md_dec"],
                 `:=`(ndec_fem = i.ndec_fem, 
                      ndec_hom = i.ndec_hom), 
                 on = c("ADEC", "md_dec")]

deces_jours_1820[is.na(ndec), ndec := 0]
deces_jours_1820[is.na(ndec_fem), ndec_fem := 0]
deces_jours_1820[is.na(ndec_hom), ndec_hom := 0]



# 2014-2017 ----
deces_1417 <- lapply(2014:2017, 
                     function(an) read.dbf(sprintf("Data/2014-2017/etatcivil%i_dec%i_dbase/dec%i.dbf", an, an, an),
                                           as.is = T) %>%
                       as.data.table()) %>%
  rbindlist() %>% data.table()

deces_1417[, c("ADEC", "MDEC", "ANAIS") := lapply(.SD, as.integer),
           .SDcols = c("adec", "mdec", "anais")]
deces_1417 <- deces_1417[, age := ADEC - ANAIS][age >= 18 & age < 30 &
                                                  !(depdec %in% 971:978)]

# Nombre de morts journalier imputé à partir du nombre de morts mensuel
nbjours_mois <- data.table(
  date_dec = seq(ymd("2014-01-01"), ymd("2017-12-31"), 
                 by = "days"))[, .(nbjours = .N), 
                               keyby = .(ADEC = as.numeric(format(date_dec, "%Y")),
                                         MDEC = as.numeric(format(date_dec, "%m")))] 

deces_1417[nbjours_mois, nbjours_mois := i.nbjours, , on = c("ADEC", "MDEC")
][, ndec_jour := .N/nbjours_mois, keyby = .(ADEC, MDEC)]


deces_1417[deces_1417[, .(ndec = as.numeric(.N/nbjours_mois)), 
                      keyby = .(ADEC, MDEC, 
                                SEXE = factor(sexe, labels = c("M", "F")))] %>%
             unique() %>%
             dcast(ADEC + MDEC ~ SEXE, value.var = "ndec"),
           `:=`(ndec_fem = i.F, ndec_hom = i.M), 
           on = c("ADEC", "MDEC")]


deces_jours_1417 <- merge(
  data.table(date_dec = seq(ymd("2014-01-01"), ymd("2017-12-31"), by = "days")
  )[, `:=`(ADEC = as.numeric(format(date_dec, "%Y")), 
           MDEC = as.numeric(format(date_dec, "%m")))],
  deces_1417[, .(ADEC, MDEC, ndec = ndec_jour, 
                 ndec_fem, ndec_hom)] %>% unique(),
  by = c("ADEC", "MDEC"), all.x = T)
deces_jours_1417[, md_dec := format(date_dec, "%m-%d")]


deces_jours <- rbind(deces_jours_1820,
                     deces_jours_1417)


# Moyenne 2014-2019 ----
deces_jours <- rbind(deces_jours,
                     deces_jours[ADEC %in% 2014:2019, 
                                 .(ndec = mean(ndec),
                                   ndec_fem = mean(ndec_fem),
                                   ndec_hom = mean(ndec_hom),
                                   ADEC = "2014-2019",
                                   MDEC = unique(MDEC)), 
                                 keyby = md_dec], fill = T)


# Périodes :
# 1 : 1er janvier - 16 mars
# 2 : 17 mars - 10 mai
# 3 : 11 mai - 29 octobre
# 4 : 30 octobre - 14 décembre
# 5 : 15 décembre - 31 décembre

deces_jours[, periode := fcase(md_dec >= "01-01" & md_dec <= "03-16", 1,
                               md_dec >= "03-17" & md_dec <= "05-10", 2,
                               md_dec >= "05-11" & md_dec <= "10-29", 3,
                               md_dec >= "10-30" & md_dec <= "12-14", 4,
                               md_dec >= "12-15" & md_dec <= "12-31", 5)]

labs_per <- c("1 jan - 16 mar", "17 mar - 10 mai", "11 mai - 29 oct",
              "30 oct - 14 déc", "15 déc - 31 déc")

# Nom du jour de la semaine, jour de weekend
deces_jours[, weekday_dec := format(date_dec, "%a")]
deces_jours[, weekend := fifelse(weekday_dec %in% c("Sat", "Sun"), 1, 0)]


# fwrite(deces_jours, "Data/deces_jours_2014-2020.csv")

