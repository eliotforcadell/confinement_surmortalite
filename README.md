# confinement_surmortalite

Ce dépôt contient l'ensemble des données et des scripts ayant permis la réalisation du mémoire *Un effet inattendu du confinement : une chute de la surmortalité violente chez les jeunes ?* par Abel Aussant, Eliot Forcadell, et Ariane Sessego.

## Mémoire

Le texte et les traitements statistiques du mémoire ont été compilés dans un même document à l'aide du système R Sweave. L'ensemble du mémoire est ainsi reproductible à partir du fichier Aussant_Forcadell_Sessego.Rnw, associé aux fichiers bibliographiques Aussant_Forcadell_Sessego_biblio.bib, Aussant_Forcadell_Sessego.bbl et Aussant_Forcadell_Sessego.bcf, et à la base de données deces_2014-2020.csv présentée ci-après.


## Données

Dans /Data :

- 2022-02-25_detail_2022 : bases et documentation pour les données de 2018 à 2020 téléchargées sur le site de l'INSEE.
- 2014-2017 : bases pour les données de 2014 à 2017, téléchargées (année par année) sur le site de l'INSEE
- crea_base.R : script utilisé pour l'empilement des bases, la création de trois nouvelles variables (dates de naissance et de décès facilement manipulables avec le package `lubridate`, âge au moment du décès), et la création de la base de données définitive avec agrégation du nombre de décès par jours (deces_2014-2020.csv).
- deces_2014-2020.csv : base de données créée avec le script crea_base.R et utilisée dans les analyses 

### Dictionnaire des variables de la base agrégée

Chaque observation correspond à un jour de l'année.

- date_dec : date de décès (aaaa-mm-jj)
- ndec : nombre de décès
- ADEC : année de décès
- MDEC : mois de décè
- md_dec : mois et jour de décès (mm-jj)
- ndec_fem : nombre de femmes décédées
- ndec_hom : nombre d'hommes décédés
- periode : périodes relatives aux confinements telles que définies dans le mémoire :
  1. Avant confinement : 1er janvier - 16 mars
  1. Premier confinement : 17 mars - 10 mai
  1. Entre-deux-confinements : 11 mai - 29 octobre
  1. Deuxième confinement : 30 octobre - 14 décembre
  1. Deuxième confinement : 15 octobre - 31 décembre
- weekday_dec : jour de la semaine (abréviations anglaises)
- weekend : indicatrice valant 1 pour un jour de week-end


### Dictionnaire des variables des bases d'origine

Chaque observation correspond à une personne décédée.

- ADEC : année de décès
- MDEC : mois de décès
- JDEC : jour de décès
- DEPDEC : département de décès
- COMDEC : commune de décès
- ANAIS : année de naissance
- MNAIS : mois de naissance
- JNAIS : jour de naissance
- SEXE : sexe
- COMDOM : commune de domicile
- LIEUDEC2 : lieu de décès  

Variables ajoutées :

- date_dec: date de décès (aaaa-mm-jj)
- date_nais : date de naissance (aaaa-mm-jj, janvier quand mois manquant, 1er quand jour manquant)
- age : âge au moment du décès

