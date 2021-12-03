tronque <- function(x) {
  if (x>=0) x else 0
}

tirer_valeur <- function() {
  return(runif(1))
}

tirer_cartes <- function() {
  return(list(
    page = floor(runif(1, 1, 16)),
    ligne = floor(runif(1, 1, 4)),
    colonne = floor(runif(1, 1, 4))
  ))
}
