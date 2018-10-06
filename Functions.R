#### Exercice 3 ----

## 3.a ----

# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL

sum_column <- function(d, var) {
  result <- NULL
  x <- d[[var]]
  if (!is.null(x)) { 
    
    if (is.numeric(x)) 
      result <- sum(x)
  }
  return(result)
}

print(sum_column(iris, "Sepal.Length"))
print(sum_column(iris, "Species"))
print(sum_column(warpbreaks, "breaks"))

## 3.b ----

# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL

my_sum <- function(x) {
  # On assigne à "my_sum" la fonction ayant x pour argument
  if (is.numeric(x)){
    # On établit une condition ayant comme contrainte "x est numérique"
    result <- 0 
    # et un résultat par défaut (0)
    for (i in x){
      # Ouverture d'une boucle qui s'applique successivement à toutes les valeurs contenues dans x
      result <- result + i }
    # On assigne un nouveau résultat tel que pour la première occurence de i dans x,
    # ce résultat soit égal à O + i. La boucle permet une addition incrémentale, à chaque
    # nouvelle valeur contenue dans x, celle ci s'ajoute à la somme précédente et ainsi de suite.
    return(result)
    # On obtient le résultat final lorsque la boucle a traité l'ensemble des i de x.
  }
  else {return(NULL)}
  # Si les valeurs ne sont pas numériques, on obtient "NULL"
}

print(my_sum(iris$Sepal.Length))
print(my_sum(iris$Species))
print(my_sum(warpbreaks$breaks))

## 3.c ----

# Quotient d'une somme par un nombre
#
# ARGUMENTS:
# x: vecteur
# k: nombre 
#
# RETURN VALUE:
# Si x et k sont numériques, retourne le quotient de la somme des valeurs 
# contenues dans le vecteur x et de k. Sinon, NULL. 

sum_divided_by <- function(x, k){
# On assigne à "sum_divided_by" une fonction à deux arguments
    if (is.numeric(x) & is.numeric(k)){
# On établit une double condition : x ∧ k = TRUE
    result <- (my_sum(x)/k)
# Pour cette condition, on paramètre le résultat en reprenant celui de la fonction my_sum,
# et en le divisant par k
    }   
    else {return(NULL)}
# Si x et k ne sont pas tous les deux numériques, alors obtient NULL
}

print(sum_divided_by(iris$Sepal.Length, 12))
print(sum_divided_by(iris$Species, 22))
print(sum_divided_by(iris$Sepal.Length, "Not numeric"))
print(sum_divided_by(warpbreaks$breaks, -12))

## 3.d ----

# Moyenne d'un vecteur
#
# ARGUMENTS:
# x: vecteur
# k: nombre 
#
# RETURN VALUE:
# Si x est numérique, retourne le quotient de la somme de x par sa longueur 
# Sinon, NULL. 

my_mean <- function(x){
# On assigne à "my_mean" une fonction à un argument
  if (is.numeric(x)) {
# On établit une condition tel que : x est numérique
    y <- length(x)
# On assigne à la variable y le nombre correspondant à la longueur du vecteur x
    result <- sum_divided_by(x, y) 
# On paramètre notre résultat en réutilisant la foncntion "sum_divides_by",
# pour obtenir le quotient de la somme du vecteur par sa longueur
    }   
  else {return(NULL)}
# Si x n'est pas numérique, NULL
  }
      
cols  
print(my_mean(iris$Sepal.Length))
print(my_mean(iris$Species))
print(my_mean(warpbreaks$breaks))

#### Exercice 4 ----

## 4.a ----

# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
# string
# grouping_var: the name of a column of d containing a grouping variable,
# provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.

grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))
  p <- p + ggplot2::geom_violin()
  return(p)
}

print(grouped_violin_plot(iris, "Sepal.Length", "Species"))

#### Exercice 5 ----

## 5.a ----

# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  
  result <- median(d_1[[var]]) - median(d_2[[var]])
  
  return(result)
}

difference_in_medians(iris, "Sepal.Width", "Species", "versicolor", "virginica")
difference_in_medians(iris, "Sepal.Width", "Species", "virginica", "virginica")

## 5.b ----

# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
# provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  n <- nrow(d)
  d[[var]] <- sample(d[[var]], n)
    return(d)
}

iris$Sepal.Width[1:10]
if(!exists(".Random.seed")) set.seed(NULL)
previous_seed <- .Random.seed
set.seed(1)
randomize(iris, "Sepal.Width")$Sepal.Width[1:10]
randomize(iris, "Species")$Species[1:10]
randomize(iris, "Species")$Sepal.Width[1:10]
set.seed(previous_seed)

## 5.c ----

# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
# provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
# a data frame, the name of a variable on which to calculate the
# test statistic, the name of a grouping variable, the value of
# the grouping variable corresponding to the first group, and
# the value of the grouping variable corresponding to the second
# group
# n_samples: the number of permutation samples to draw (default: 9999)
#

# RETURN VALUE:
#
# A list containing two elements:
#
# - observed: the value of statistic() in d
# - permuted: a vector containing the values of statistic() under n_samples
# permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    # fill in the vector permutation_statistics with the
    # value of statistic(...) for this new permutation
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

## 5.d ----

## 5.e ----




