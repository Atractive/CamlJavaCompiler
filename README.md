# Projet Compilateur OCaml -> Java
## Utilisation
Le dossier *caml_files* est celui contenant les fichiers pour la compilation, notamment le **Makefile** et le **comp**
Les tests et les fichiers Java sont chacun dans leur propre dossier, il faut ainsi faire `./comp ../Tests/test.ml ../java_files/Gen.java` pour générer le fichier Java, si on se situe dans le dossier des fichiers caml.

En se servant d'OCaml:
- `parse` permet de parser le programme
- `compile_prog` permet de le compiler
- `execute` permet de l'exécuter (c'est un raccourci pour la fonction `exec`)


## Travail réalisé
- Traduction du fragment fonctionnel (fonctions mutuellement récursives comprises)
- Génération du fichier Gen.java
- Exécution du fichier compilé en Java

## Problèmes rencontrés
- Celui de la priorité, que j'ai réussi à résoudre en rajoutant des grammaires
- Celui de la suppression d'éléments du code ou du stack en Java avec la fonction `pop()`. En effet, il fallait bien utiliser le constructeur de `LinkedList`, mais il fallait s'en servir à bien plus d'endroit que ce que j'imaginais
- Compiler l'instruction `Fix`. Cela a nécessité une définition locale, ainsi que deux fonctions (une pour ajouter les noms des fonctions à l'environnement, une autre pour compiler leur corps) dont une mutuellement récursive (une d'elle faisait un appel à la fonction `compile`)

## Tests effectués
Le code a été testé et a fonctionné pour tous les tests présents dans le dossier *Tests*. Les tests pour les fonctions d'Ackermann faisaient énormément d'appels récursifs, ce qui pourrait causer des problèmes avec des nombres trop élevés.



### Auteur
Benjamin Bardy
