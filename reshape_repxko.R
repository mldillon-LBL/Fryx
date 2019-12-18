#reshape relative abundance KO by replicate table.

koxrep <- read.csv("~/Documents/Fryxell/Functional Fryxell/Humann2/cat_gene_families_cpm_uniref50_ko_unique.csv")

library(reshape2)
koxrep.long <- melt(koxrep, id = 1, measure = 2:length(koxrep))
names(koxrep.long)[1] <- "KO"
names(koxrep.long)[2] <- "Replicate"
names(koxrep.long)[3] <- "CPM"

koxrep.long$Depth <- rep("NA", dim(koxrep.long)[1])
koxrep.long$Layer <- rep("NA", dim(koxrep.long)[1])

koxrep.long$Depth[grep("10.4", koxrep.long$Replicate) ] <- 9.8
koxrep.long$Depth[grep("HC", koxrep.long$Replicate) ] <- 9.3
koxrep.long$Depth[grep("9.4", koxrep.long$Replicate) ] <- 9.0

koxrep.long[sample(nrow(koxrep.long), 10), ]

write.csv(x = koxrep.long, file = "~/Documents/Fryxell/Functional Fryxell/Humann2/cat_gene_families_cpm_uniref50_ko_unique.long.csv")

#format cpm table

samplexgene_cpm <- koxrep_cpm_t46 %>%
  select(Sample, CPM, KEGG_Description) %>%
  group_by(Sample, KEGG_Description) %>%
  summarise(average_CPM = mean(CPM)) %>%
  spread(KEGG_Description, average_CPM)