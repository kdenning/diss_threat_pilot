

dem_percent_table <- function(counts_table) {
  counts_table %>% 
    mutate(n_total = sum(counts_table$n),
         percent = n/n_total*100) %>% 
    select(-n_total)
}


