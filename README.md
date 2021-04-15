# Projet de série linéaire temporelle

## Présentation du projet

Ceci est le projet de série témporelles pour le cours de 2A de l'ENSAE, portant sur la modélisation ARIMA d'un indice de la production industrielle.

## Consignes

Le [sujet][sujet] du devoir (en pdf) est dans le repo GitHub.

### À rendre

* au plus tard le **14 mai 2020** en le déposant dans ce lien [Dropbox][dropbox] (pas d'inscription nécessaire)
* dans un dossier compressé (.zip de préférence) :
  * nommé *"pnom1pnom2"* où *"pnom1"* est la 1<sup>ère</sup> lettre du prénom et le nom du membre 1 du binôme. Ajoutez *"_en"* à la fin si vous rédigez le rapport en anglais.
  * contenant **le rapport au format PDF**, **le script .R** et **la série utilisée au format CSV**.

### Consignes générales

Ce projet doit être réalisé sous R. La notation tiendra compte de la rigueur dans la mise en œuvre des outils économétriques, de la concision et de la clarté de la présentation des résultats. Le rapport (6 pages de contenu maximum hors annexe, en français ou en anglais) fera figurer les programmes (commentés) en annexe. Ce tutorat doit être effectué seul ou (de préférence) en binôme. Le barème (à titre indicatif) est le suivant : Partie 1 : 30%; Partie 2 : 30%; Partie 3 : 40%.

On s'intéresse à la modélisation et la prévision de l'indice de production industrielle observé en France. Il ne vous est pas demandé de connaître le processus de construction de l'indicateur. Vous ne travaillez que sur les données observées. À partir du répertoire des séries chronologiques de l'INSEE ([lien des séries de l'IPI][repertoire]), vous devez choisir une série agrégée corrigée des variations saisonnières et des jours ouvrés (CVS-CJO), mensuelle, correspondant à n'importe quel secteur d'activité (à votre convenance) et contenant au moins 100 observations.

## Données

[Répertoire][repertoire] de l'INSEE contenant les séries.

Lien de la série choisie : [Indice CVS-CJO de la production industrielle (base 100 en 2015) - Fabrication de gaz industriels (NAF rév. 2, niveau classe, poste 20.11)][serie].

Téléchargement de la série choisie au format [CSV][csv].

## Graphique

![ts_plot.png][plot]

<!-->
<!-->
<!-->
<!--Liens-->
[sujet]: ./projet_seriestemp_2021.pdf
[dropbox]: https://www.dropbox.com/request/8MGe3M2ctrmaCcCooGHh
[repertoire]: https://www.insee.fr/fr/information/3128533?CORRECTION=2238608&INDICATEUR=2765760&PERIODICITE=2224021
[serie]: https://www.insee.fr/fr/statistiques/serie/010537426
[csv]: https://www.insee.fr/fr/statistiques/serie/telecharger/010537426?ordre=antechronologique&transposition=donneescolonne&periodeDebut=1&anneeDebut=1990&periodeFin=1&anneeFin=2021
[plot]: ./output/images/ts_plot.png
