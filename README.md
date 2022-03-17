# confinement_surmortalite

## Données

Dans /Data :

- les bases et la documentation téléchargées sur le site de l'INSEE.
- crea_base.R : script utilisé pour l'empilement des bases et la création de trois nouvelles variables : dates de naissance et de décès facilement manipulables avec le package `lubridate`, et âge au moment du décès.

### Dictionnaire des variables 

- ADEC : Année de décès
- MDEC : Mois de décès
- JDEC : Jour de décès
- DEPDEC : Département de décès
- COMDEC : Commune de décès
- ANAIS : Année de naissance
- MNAIS : Mois de naissance
- JNAIS : Jour de naissance
- SEXE : Sexe
- COMDOM : Commune de domicile
- LIEUDEC2 : Lieu de décès  

Variables ajoutées :

- date_dec: date de décès (aaaa-mm-jj)
- date_nais : date de naissance (aaaa-mm-jj, janvier quand mois manquant, 1er quand jour manquant)
- age : âge au moment du décès

