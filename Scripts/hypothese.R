pacman::p_load(data.table, ggplot2, foreign)
theme_set(theme_minimal())

deces <- fread("Data/deces_2018-2022.csv")[ADEC %in% 2018:2020 & 
                                             age >= 18 & age < 30 & 
                                             date_dec != "2020-02-29"]

# Périodes :
# 1 : 1er janvier - 16 mars
# 2 : 17 mars - 10 mai
# 3 : 11 mai - 29 octobre
# 4 : 30 octobre - 14 décembre
# 5 : 15 décembre - 31 décembre

deces[, md_dec := format(date_dec, "%m-%d")]
deces[, periode := fcase(md_dec >= "01-01" & md_dec <= "03-16", 1,
                         md_dec >= "03-17" & md_dec <= "05-10", 2,
                         md_dec >= "05-11" & md_dec <= "10-29", 3,
                         md_dec >= "10-30" & md_dec <= "12-14", 4,
                         md_dec >= "12-15" & md_dec <= "12-31", 5)]

# Numéro de la semaine
deces[, semaine_dec := isoweek(date_dec)]
deces[semaine_dec == 1 & MDEC == 12, semaine_dec := 53]
deces[ADEC == 2018 & semaine_dec == 53, unique(date_dec)]

# Numéro du jour de l'année et nom du jour de la semaine
corresp_numjour <- data.table(md_dec = format(seq(ymd("2018-01-01"), ymd("2018-12-31"), 
                                              by="days"), "%m-%d"),
                              numjour = 1:365)
deces[corresp_numjour, numjour := i.numjour, on = "md_dec"]
deces[, weekday_dec := weekdays(date_dec)]

# Nombre de jours par période
# deces[, length(unique(date_dec)), keyby = .(periode, ADEC)]


# Manque le 28 avril chez les 18-29 ans
# setdiff(deces[ADEC == 2018 & age >= 18 & age < 30, unique(md_dec)],
#         deces[ADEC == 2020 & age >= 18 & age < 30, unique(md_dec)])

# Correspondance semaine / période de confinement
# lapply(2018:2020, function(a) deces[periode == 2 & ADEC == a, 
#                                     .(semaine_dec, date_dec)] %>% unique())


deces_jour <- deces[, .(ndec = .N,
                        MDEC,
                        numjour,
                        date_dec,
                        periode,
                        weekday_dec), 
                    keyby = .(md_dec, ADEC)] %>% unique()

## Gaussian smoothing du nombre de morts par jour sur une fenêtre de 30 jours
deces_jour[data.table(date_dec = sort(deces_jour$date_dec),
                 smooth_dec = smth.gaussian(deces_jour[order(date_dec), ndec],
                                            window = 30, alpha = 2.5)),
           smooth_dec := i.smooth_dec,
           on = "date_dec"]

ggplot(deces_jour) +
  geom_rect(aes(xmin = deces[md_dec == "03-17", unique(numjour)], 
                xmax = deces[md_dec == "05-10", unique(numjour)], 
                ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "grey90") +
  geom_rect(aes(xmin = deces[md_dec == "10-30", unique(numjour)], 
                xmax = deces[md_dec == "12-14", unique(numjour)], 
                ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "grey90") +
  geom_vline(xintercept = deces[md_dec == "03-17", unique(numjour)], colour = "grey80") +
  geom_vline(xintercept = deces[md_dec == "05-10", unique(numjour)], colour = "grey80") +
  geom_vline(xintercept = deces[md_dec == "10-30", unique(numjour)], colour = "grey80") +
  geom_vline(xintercept = deces[md_dec == "12-14", unique(numjour)], colour = "grey80") +
  geom_line(aes(x = numjour, 
                y = smooth_dec,
                group = ADEC,
                colour = factor(ADEC))) +
  ylim(5,15) +
  scale_colour_viridis_d("Année", end = 0.8, option = "F") +
  labs(y = "Nombre de morts", x = "Jour")


## Données sur 2014-2017
deces_1417 <- lapply(2014:2017, 
                     function(an) read.dbf(sprintf("Data/2014-2017/etatcivil%i_dec%i_dbase/dec%i.dbf", an, an, an),
                                           as.is = T) %>%
                       as.data.table()) %>%
  rbindlist()

deces_1417[, c("ADEC", "MDEC", "ANAIS") := lapply(.SD, as.integer),
           .SDcols = c("adec", "mdec", "anais")]
deces_1417[, age := ADEC - ANAIS]


## Nombre de décès par mois et années
deces_ym <- rbind(deces_1417[age >= 18 & age < 30, .(ndec = .N), keyby = .(ADEC, MDEC)],
                  deces[, .(ndec = .N), keyby = .(ADEC, MDEC)])

## Comparaison des courbes pour chaque années (moche)
ggplot(deces_ym) +
  geom_line(aes(x = MDEC, y = ndec, 
                group = ADEC, colour = factor(ADEC))) +
  scale_x_continuous(breaks = 1:12)



## Comparaison 2020 vs moyenne 2014-2019
rbind(deces_ym[ADEC != 2020, .(ndec = mean(ndec),
                               ADEC = "2014-2019",
                               sd = sd(ndec)), keyby = MDEC
               ][, `:=`(ci_low = ndec - 1.96*sd,
                        ci_high = ndec + 1.96*sd)],
      deces_ym[ADEC == 2020], fill = T) %>%
  ggplot() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, x = MDEC, group = ADEC), 
            linetype = "blank", fill = "orange", alpha = 0.1) +
  geom_line(aes(x = MDEC, y = ndec, 
                group = ADEC, colour = factor(ADEC))) +
  geom_line(aes(y = ci_low, x = MDEC, group = ADEC), 
            linetype = "dashed", colour = "orange") +
  geom_line(aes(y = ci_high, x = MDEC, group = ADEC), 
            linetype = "dashed", colour = "orange") +
  scale_colour_manual("Années", values = c("orange", "cornflowerblue")) +
  scale_x_continuous(breaks = 1:12)


## Essais de lissage avec imputation du nombre de morts journalier
## sur les années 2014-2019

deces_1417[deces[, length(unique(md_dec)), keyby = MDEC],
           m_nbjours := i.V1, on = "MDEC"]

deces_1417_jour <- data.table(date_dec = as.IDate(seq(ymd("2014-01-01"), ymd("2017-12-31"), by="days"))
)[, `:=`(MDEC = as.integer(format(date_dec, "%m")),
         ADEC = as.integer(format(date_dec, "%Y")),
         md_dec = format(date_dec, "%m-%d"))
  ][corresp_numjour, numjour := i.numjour, on = "md_dec"
    ][deces_1417[age >= 18 & age < 30, 
                 .(ndec = .N/m_nbjours), keyby = .(ADEC, MDEC)] %>% unique(),
      ndec := i.ndec, on = c("ADEC", "MDEC")]

deces_jour_full <- rbind(deces_1417_jour[md_dec != "02-29"],
                         deces_jour[, .(date_dec, MDEC, ADEC, numjour, ndec)],
                         fill = T)

deces_jour_full[data.table(date_dec = sort(deces_jour_full$date_dec),
                      smooth_dec = smth.gaussian(deces_jour_full[order(date_dec), ndec],
                                                 window = 30, alpha = 2.5)),
           smooth_dec := i.smooth_dec,
           on = "date_dec"]

ggplot(deces_jour_full) +
  geom_rect(aes(xmin = deces[md_dec == "03-17", unique(numjour)], 
                xmax = deces[md_dec == "05-10", unique(numjour)], 
                ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "grey90") +
  geom_rect(aes(xmin = deces[md_dec == "10-30", unique(numjour)], 
                xmax = deces[md_dec == "12-14", unique(numjour)], 
                ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "grey90") +
  geom_vline(xintercept = deces[md_dec == "03-17", unique(numjour)], colour = "grey80") +
  geom_vline(xintercept = deces[md_dec == "05-10", unique(numjour)], colour = "grey80") +
  geom_vline(xintercept = deces[md_dec == "10-30", unique(numjour)], colour = "grey80") +
  geom_vline(xintercept = deces[md_dec == "12-14", unique(numjour)], colour = "grey80") +
  geom_line(aes(x = numjour, 
                y = smooth_dec,
                group = ADEC,
                colour = factor(ADEC))) +
  ylim(5,15) +
  scale_colour_viridis_d("Année", end = 0.8, option = "F") +
  labs(y = "Nombre de morts", x = "Jour")
