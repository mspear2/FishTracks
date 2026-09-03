library(here)
library(tidyverse)

int <- read.csv(here('data', 'Non-animal tags from Appel', 'intermediate.csv'))

int$TagID

int <- int %>%
  mutate(
    tag_code_space = str_extract(TagID, '(^...-....)-.*$', 1),
    tag_id_code =  str_extract(TagID, '^...-....-(.*$)', 1)
  ) %>%
  distinct() %>%
  select(-TagID)


write.csv(int, here('data', 'Non-animal tags from Appel', 'Tx, Nextrak, and unknown matches_clean.csv'))


int
