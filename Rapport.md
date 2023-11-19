---
title: Rapport Projet TAS - Implémentation d'un langage de programmation avec typage
subtitle: Typeur, Evaluateur , Parser
author: Dibassi Brahima 21210230
date: 19 Avril, 2023
lang: fr
geometry:
  - margin = 1.2in
mainfont: Palatino
sansfont: Helvetica
monofont: Menlo
fontsize: 12pt
urlcolor: NavyBlue
include-before: | # Texte avant la table des matières
    \newpage
numbersections: true # Numéros de sections
toc: true # Table des matières
tableofcontents: true # Table des matières
---
\newpage

# Langage de programmation choisi

Afin d'implémenter ce projet, nous avons choisi le langage de programmation *OCaml*.\
Ayant déjà expérimenté l'implémentation de l'interpréteur d'un langage en OCaml suite au cours d' **APS**, nous avons pu constater que ce langage était très adapté à ce genre de projet.\
Grâce à la somme de produits au pattern-matching associé, et au support des paradigmes, imperatif et fonctionnel, nous avons pu nous servir des différents traits du langage afin de faciliter notre implémentation de l'évaluateur et du typeur.

# Lancement du projet

Afin de compiler notre projet il suffit d'être à la racine de chaque version d'APS et de lancer :
```sh
    dune build
```
Pour les versions du langage supérieures à APS1A, nous proposons également un lancement automatisé de notre batterie de tests. La commande est :
```sh
    dune test --force
```

Si vous souhaitez lancer votre fichier à travers toutes les étapes, il est également possible d'utiliser la commande suivante :
```sh
    dune exec ProjetTAS *chemin vers le fichier à tester*/*fichier*.ml
```

\newpage

# Structure du projet
- Repertoire `Lib/`
  - `lexer.mll` : Création des tokens
  - `parser.mly` : Parser afin de construire notre arbre syntaxique abstrait depuis les tokens
  - `ast.ml` : Structure de l'AST du langage avec les types des expressions
  - `baselib.ml` : Définitions des primitives du langage et leur types
  - `evaluator.ml` : Partie sémentique du langage, évaluation des expressions
  - `prettyprinter.ml` : Fonctions d'affichage
  - `typeur.ml` : Partie analyse statique, vérification des types
  - `typingEnv.ml` : Gestion de l'environnement des déclarations de types par l'utilisateur

- Repertoire `Bin/`
  - `main.ml` : Contient notre script principal, lancé lors de la commande `dune exec ProjetTAS *fichier*.ml`. Il vérifie si le programme .ml ciblé est bien typé et l'évalue si c'est le cas en faisant tous les affichages sur le terminal. Nous avons choisi d'afficher également l'état de la mémoire à la fin de l'évaluation du programme.

- Repertoire `Test/`
  - `testing.ml` : Script de test, permet de tester tous les fichiers .ml présents dans le dossier `test` et de vérifier si le typeur et l'évaluateur fonctionnent correctement.
  - `yamlHelper.ml` : Fonctions d'aide pour la lecture des fichiers de test au format yaml
  - `template.yaml` : Template de fichier de test au format yaml

Les tests sont dans le dossier `test` classés en fonction des extentions du langage auxquels ils correspondent.
Les extensions du langage devant également passer les tests des versions précédentes.

\newpage

# Syntaxe du langage

# Difficultés rencontrées

# Extensions
## Types Utilisateurs
## MatchPattern
## Annotations de type
## Gestion des erreurs 

-- TODO POLY FAIBLE
-- TODO EQUALITY STRUCT
