##Dataset


dataset1 <- readRDS('Classeur1.rds')

dataset <- data.frame(dataset1)

fields <- colnames(dataset)
fields <- fields[!fields %in% c("etape", "Exemple.de.DM", "recommandations.pratiques", "action1", "action2")]


## Filters : quosure and condition - https://github.com/keithmcnulty/summer_olympics/blob/master/index.Rmd

filter1_by <- function(df, f1, fv1) {
  filter_f1 <- quo(all_of(f1))
  
  df %>%
    filter_at(vars(!!filter_f1), all_vars(. == fv1))
}


filter2_by <- function(df, f1, fv1, f2, fv2) {
  filter_f1 <- quo(f1)
  filter_f2 <- quo(f2)
  
  df %>%
    filter_at(vars(!!filter_f1), all_vars(. == fv1)) %>%
    filter_at(vars(!!filter_f2), all_vars(. == fv2))
}


filter3_by <- function(df, f1, fv1, f2, fv2, f3, fv3) {
  filter_f1 <- quo(f1)
  filter_f2 <- quo(f2)
  filter_f3 <- quo(f3)
  
  df %>%
    filter_at(vars(!!filter_f1), all_vars(. == fv1)) %>%
    filter_at(vars(!!filter_f2), all_vars(. == fv2)) %>%
    filter_at(vars(!!filter_f3), all_vars(. == fv3))
  
}

