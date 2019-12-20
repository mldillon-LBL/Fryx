library(tidyverse)

#I want just 9.0 m

t_46_cpm <- read_tsv(file = "t46_cpm.tsv")

amy_rbc_psb_9 <- t_46_cpm %>%
  filter(Depth == 9.0, Gene.clusters == "psb" | Gene.clusters == "rbc" | Gene.clusters == "amy") %>%
  select(Gene.clusters, CPM, Replicate) %>%
  pivot_wider(names_from = Gene.clusters, values_from = CPM, values_fill = list(CPM = 0), values_fn = list(CPM = mean))
  