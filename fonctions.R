tronque <- function(x) {
  if (x>=0) x else 0
}

tirer_valeur <- function() {
  return(runif(1))
}

tirer_cartes <- function(table) {
  return(table[sample(1:nrow(table), 1), ])
}

cartes_tsv <- read.delim("./cartes.tsv", header = FALSE)

cartes <- bind_rows(
  transmute(cartes_tsv, Gauche = V1, Droite = V2),
  transmute(cartes_tsv, Gauche = V3, Droite = V4),
  transmute(cartes_tsv, Gauche = V5, Droite = V6),
  transmute(cartes_tsv, Gauche = V7, Droite = V8),
  ) %>% 
  filter(Gauche != "") %>% 
  mutate(across(c(Gauche, Droite), str_replace, "-[:space:]+", "")) %>% 
  mutate(across(c(Gauche, Droite), str_replace, "^[:space:]", "")) %>% 
  mutate(across(c(Gauche, Droite), str_replace, "[:space:][:space:]+", " "))
