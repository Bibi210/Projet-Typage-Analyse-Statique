---
title: Rapport Projet TAS - Implémentation d'un langage de programmation avec typage
subtitle: Typeur, Evaluateur , Analyseur Syntaxique 
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

\newcommand{\grammarRule}[1]{\; \textbf{<\textcolor{Blue}{#1}>} \;}
\newcommand{\grammarRuleUnSpaced}[1]{\textbf{<\textcolor{Blue}{#1}>}}
\newcommand{\nTime}[1]{\; #1\text{*} \;}
\newcommand{\nPlus}[1]{\; #1\text{+} \;}
\newcommand{\isToken}[1]{\; \textit{`\textcolor{Maroon}{#1}`} \;}
\newcommand{\isRangeToken}[2]{\; \textit{`\textcolor{Maroon}{#1} - \textcolor{Maroon}{#2}`} \;}

# Langage de programmation choisi

Afin d'implémenter ce projet, nous avons choisi le langage de programmation *OCaml*.\
Ayant déjà expérimenté l'implémentation de l'interpréteur d'un langage en OCaml suite au cours d' **APS**, nous avons pu constater que ce langage était très adapté à ce genre de projet.\
Grâce à la somme de produits au pattern-matching associé, et au support des paradigmes, impératif et fonctionnel, nous avons pu nous servir des différents traits du langage afin de faciliter notre implémentation de l'évaluateur et du typeur.

# Lancement du projet

Si vous souhaitez lancer votre fichier utilisez la commande suivante :
```sh
    dune exec ProjetTAS *chemin vers le fichier à tester*/*fichier*.ml
```

Nous proposons également un lancement automatisé de notre batterie de tests. La commande est :
```sh
    dune test --force
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
Les extensions du langage doivent également passer les tests des versions précédentes.

\newpage

# Syntaxe du langage

Une grande partie de la syntaxe du langage est reprise d'un projet précédent, le langage MiniML.\

## Programme
On définit un programme comme une expression précédée de zéro ou plusieurs définitions néccessaires à son évaluation.
\begin{align*}
      \grammarRule{Prog} ::= \quad & |\quad \grammarRule{Expr}                                  \\
                                   & |\quad \grammarRule{Def}  \isToken{;;}  \grammarRule{Prog}
\end{align*}

## Elément nommés

On appelle éléments nommés, les identificateurs \grammarRule{Id} pour les variables, motifs et types, les identificateurs de constructeurs \grammarRule{ConstructeurId} et les variables de types \grammarRule{Vartype}

\begin{align*}
      \grammarRule{Id} ::= \quad             & \nPlus{[\isRangeToken{a}{z} \isRangeToken{A}{Z} \isRangeToken{0}{9} \isToken{\_}]} \\
      \grammarRule{ConstructeurId} ::= \quad & [\isRangeToken{A}{Z}] \grammarRule{Id}                                             \\
      \grammarRule{Vartype} ::= \quad        & \isToken{'}[\isRangeToken{a}{z}] \; \nTime{[\isRangeToken{0}{9}]}
\end{align*}

## Types

\begin{itemize}
      \tightlist
      \item
            Les \textbf{Types polymorphique}.
      \item
            Les \textbf{Utilisation de variables de types}.
      \item
            Les \textbf{Applications de types}.
      \item
            Les \textbf{Lambda}.
      \item
            Les \textbf{Tuples}.
\end{itemize}

\begin{align*}
      \grammarRule{Type}    \quad ::=  \quad & |\quad \grammarRule{Vartype}                              \\
                                             & |\quad \grammarRule{Id}                                   \\
                                             & |\quad \grammarRule{Type}   \grammarRule{Type}            \\
                                             & |\quad \grammarRule{Type} \isToken{->} \grammarRule{Type} \\
                                             & |\quad \grammarRule{Type} \isToken{*}  \grammarRule{Type} \\
\end{align*}

\pagebreak

## Déclarations de types

\begin{align*}
                                  & \quad \isToken{type} \nTime{\grammarRuleUnSpaced{Vartype}} \grammarRule{Id} \isToken{=} \grammarRule{NewContructors} \\
\end{align*}

### Nouveaux constructeurs

\begin{align*}
      \grammarRule{NewContructors} ::=   \quad & |\quad  \grammarRule{ConstructeurId} \isToken{of} \grammarRule{Type}                                            \\
                                               & |\quad  \grammarRule{NewContructors} \isToken{|} \grammarRule{NewContructors}                                   \\
\end{align*}

## Expressions

\begin{itemize}
      \tightlist
      \item
            Les \textbf{Littéraux}.
      \item
            Les \textbf{Utilisations de Variables}.
      \item
            Les \textbf{Appels d'opérateurs} unaire et binaire.
      \item
            Les \textbf{Appels de fonctions}.
      \item
            Les \textbf{Tuples}.
      \item
            Les \textbf{Lambda}.
      \item
            Les \textbf{Fonctions Récursives}.
      \item
            Les \textbf{Constructions}.
      \item
            Les \textbf{Correspondance de motifs}.
\end{itemize}

\begin{align*}
      \grammarRule{Litteral}  \quad ::=  \quad       & |\quad \nPlus{[\isRangeToken{0}{9}]}                                                                                                                                                  \\
                                                     & |\quad \isToken{(} \isToken{)}                                                                                                                                                        \\
      \\
      \grammarRule{Expr}  \quad ::=  \quad           & |\quad \grammarRule{Litteral}                                                                                                                                                         \\
                                                     & |\quad \grammarRule{Id}                                                                                                                                                               \\
                                                     & |\quad \grammarRule{UnaryOperator}  \grammarRule{Expr}                                                                                                                                \\
                                                     & |\quad  \grammarRule{Expr} \grammarRule{BinaryOperator}    \grammarRule{Expr}                                                                                                         \\
                                                     & |\quad \grammarRule{Expr}  \grammarRule{Expr}                                                                                                                                         \\
                                                     & |\quad \grammarRule{Expr} \isToken{,}  \grammarRule{Expr}                                                                                                                             \\
                                                     & |\quad \isToken{fun} \grammarRule{Id} \isToken{->}  \grammarRule{Expr}                                                                                                                \\
                                                     & |\quad \isToken{let} \grammarRule{Id} \grammarRule{Id} \isToken{=}  \grammarRule{Expr}  \isToken{in}  \grammarRule{Expr}                                                                                                         \\
                        & |\quad \isToken{let} \isToken{rec}  \grammarRule{Id} \isToken{=}  \grammarRule{Expr}  \isToken{in}  \grammarRule{Expr}                                         
                                                                                        \\
                                                     & |\quad \grammarRule{ConstructeurId}  \grammarRule{Expr}                                                                                                                               \\
                                                     & |\quad \isToken{match} \grammarRule{Expr} \isToken{with} \grammarRule{MatchCase}                                                                                                      \\
\end{align*}

## Filtrage de motifs

\begin{itemize}
      \tightlist
      \item
            Les patterns sur \textbf{Littéraux}.
      \item
            Les patterns sur \textbf{Variables}.
      \item
            Les patterns sur \textbf{Tuple}.
      \item
            Les patterns sur \textbf{Constructeurs}.
\end{itemize}

\begin{align*}
      \grammarRule{MatchCase}  \quad ::=  \quad & |\quad  \grammarRule{Pattern} \isToken{->}  \grammarRule{Expr}     \\
                                                & |\quad \grammarRule{MatchCase} \isToken{|} \grammarRule{MatchCase} \\
      \\
      \grammarRule{Pattern} \quad ::=  \quad    & |\quad \grammarRule{Litteral}                                      \\
                                                & |\quad \grammarRule{Id}                                            \\
                                                & |\quad \grammarRule{Pattern}  \isToken{,} \grammarRule{Pattern}    \\
                                                & |\quad \grammarRule{ConstructeurId} \; \grammarRule{Pattern}       \\
\end{align*}

\pagebreak


# Let Polymorphique

Durant la réalisation de ce projet, nous avons rencontré une difficulté majeure, celle de la gestion du polymorphisme qui a été un véritable challenge pour nous.\

En effet, nous avons dû faire face à plusieurs problèmes :

\begin{enumerate}
      \item
            La gestion de l'environnement des types
      \item
            La généralisation des types
      \item
            La remontée des substitutions induite par l'unification
\end{enumerate}


Afin de résoudre ces problèmes, nous avons dû revoir notre gestion de l’environnement des types plusieurs fois.\

Dans un premier temps, notre environnement des types était un simple set de variables de types.
Collectant les variables de types lors de leur création. Hélas cette implémentation ne nous permettait pas de récupérer les nouvelles variables de type créées lors de la récupération des substitutions.\

La solution que nous avons mise en place afin de résoudre ce problème a été de récupérer toutes les substitutions induites par le typeur et de les appliquer à l’environnement des types.
De plus, nous ajoutons toutes les substitutions induites par la généralisation à la génération d’équations courante.

\pagebreak

# Extensions

Dans cette partie nous allons présenter les différentes extensions que nous avons implémentées dans notre langage. Pour chacune d’entre elles une implémentation de l’évaluateur et du typeur a été réalisée.

## Annotations de type

Afin de simplifier les tests sur notre typeur, nous avons implémenté la possibilité d'annoter les expressions avec leur type attendu.\

```ocaml
let intId a = (a : int) in (intId 1) (* Type *)
let intId a = (a : int) in (intId (fun a -> a)) (* Type Error *)
```

## Types Utilisateurs

Pour generaliser les cas listes chainées et option, nous avons implémenté la possibilité de définir des types utilisateurs.

```ocaml
type 'a list = 
  | Nil 
  | Cons of ('a * ('a list))

type 'a option = 
  | None
  | Some of 'a
```

Au sein du typeur, ces définitions de types sont traduites en source d’instances de type. Ainsi, lors de la vérification de type, les types utilisateurs sont traités comme des types issus d’une généralisation.
Pour pouvoir utiliser ces définitions, un environnement est créé au début du programme afin de mettre en association les constructeurs avec leur définitions et le type qu’ils construisent.

\pagebreak

## MatchPattern Profond

Afin de pouvoir utiliser correctement les types utilisateurs, nous avons implémenté la possibilité de faire du filtrage de motifs profonds.\

Il prend la forme suivante :

```ocaml
let t = Some (Some (1)) in 
match t with
| Some (Some x) -> x
| Some (Some x) -> (x : (int ref)) (* Ici ne type pas *)
| None -> 0
```
### Analyse Statique

L' analyse statique vérifie ici 4 informations :
\begin{enumerate}
      \item
            Le type des patterns est bien conforme au type de l'expression filtrée
      \item
            Le type de retour de chaque branche est bien conforme au type de retour de l'expression filtrée
      \item
            Que l'expression de retour de chaque branche n'utilise pas de variables non définies
      \item
            L'absence de doublons dans les variables de motifs
\end{enumerate}

### Evaluation

L'évaluation s'est avérée un peu plus complexe, car il a fallu mettre en place un parcours de graphe afin de pouvoir associer les variables de motifs aux valeurs correspondantes dans l'expression filtrée.

\pagebreak

## Gestion des erreurs 

Nous avons implémenté une gestion des erreurs afin de pouvoir afficher des messages d’erreurs plus explicites.\
Il est important de noter qu’on ne parle pas ici d’erreurs définies explicitement par l’utilisateur, mais d’erreurs liées à une mauvaise utilisation, à une détection par analyse statique ou à une erreur d’évaluation.

### Analyse Statique

En plus des garenties de typage, nous avons implémenté des erreurs de typage afin de pouvoir afficher des messages d'erreurs plus explicites.

- Utilisation de variables non définies
- Utilisation de constructeurs non définis
- Double utilisation de variables au sein d'un même pattern

Lorsque l'une de ces erreurs est détectée, ou lorsque l'unification échoue, nous affichons un message d'erreur explicite mentionnant la ligne et la colonne de début et de fin de l'expression concernée, de plus nous affichons l'équation qui a échoué.

**Exemples :**

```ocaml
let x = a in ()
```

Affichera :

```sh
Type Inference:
Error from line 1 col 8 to line 1 col 9: 
  Unbound variable a.
```

```ocaml
let x = (fun a -> a) in (x : int)
```

Affichera :

```sh
Type Inference:
Error from line 1 col 33 to line 1 col 36: 
  int = (lambda int,int).
```

### Erreurs d'évaluation

Nous avons également implémenté des erreurs d'évaluation afin de pouvoir afficher des messages d'erreurs plus explicites.\

- Fuite dans les cas de filtrage
- Division par zéro

Comme pour les erreurs de typage, lorsqu'une erreur d'évaluation est détectée, nous affichons un message d'erreur explicite mentionnant la ligne et la colonne de début et de fin de l'expression concernée.

**Exemples :**

```ocaml
(1 / ((fun a -> 0) () )  ) 
```

Affichera :

```sh
Evaluation:
Error from line 1 col 0 to line 1 col 26: 
  Division by zero : (1 / ((fun a -> 0) ())).
```

```ocaml
(match 2 with 
 1 -> ()
)
```

Affichera :

```sh
Evaluation:
Error from line 1 col 0 to line 1 col 22: 
  Non exhaustive pattern match on :
(match 2 with 
 1 -> ()
)
```

# Améliorations possibles

Dans cette partie, nous allons présenter les différentes améliorations que nous aurions pu implémenter dans notre langage.

Toutes ces améliorations ont été pensées et spécifiées, mais par manque de temps, nous n'avons pas pu les mettre en place.


**Erreurs Utilisateur:**

La possibilité de définir des erreurs et d'en générer au sein du programme aurait pu être une amélioration intéressante.

**Egalité structurelle :**

Nous aurions pu implémenter l'égalité structurelle afin de pouvoir comparer des valeurs de type complexe comme les constructions.

**Analyse Statique :**

Voici les différentes améliorations que nous aurions pu implémenter dans l'analyse statique :

\begin{enumerate}
      \item
            Exhaustivité du filtrage de motif
            Vérifier que le filtrage de motif était exhaustif. C'est-à-dire que toutes les valeurs possibles de l’expression filtrée sont couvertes par les patterns du filtrage de motifs.
      \item
            Détection de pattern inatteignable
            Détecter que le filtrage de motif ne contient pas de pattern inatteignable.\
            C'est-à-dire qu'un pattern n'est pas atteignable, car il est précédé d'un pattern qui couvre toutes les valeurs possibles de l'expression filtrée.
      \item
            Détection des variables non utilisées
            Lors de l'analyse statique, nous aurions pu vérifier que toutes les variables de motifs sont utilisés dans l'expression de retour de chaque branche.
      \item
            Détection des types non utilisés
            Vérifier que tous les types définis par l'utilisateur sont utilisés dans le programme.
\end{enumerate}
