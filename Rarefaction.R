library(tidyverse) 

data_path = "raw"
files <- dir(path = "raw/", pattern = "*.rarefact")

data <- data_frame(filename = files) %>% 
  mutate(file_contents = map(filename, ~ read_table(file.path(data_path, .), col_names = c("placerun", "k", "unrooted_mean", "rooted_mean", "quadratic_mean"), skip = 1)))

rarefacts <- unnest(data, cols = file_contents) %>%
  select(-filename) %>%
  separate(col = placerun, into = "replicate", extra = "drop", sep = ".cat.ereps.afu.fastq")

rarefacts$Depth <- rep("NA", nrow(rarefacts))
rarefacts$Layer <- rep("NA", nrow(rarefacts))

rarefacts$Depth[grep(9.4, rarefacts$replicate)] <- 9.0
rarefacts$Depth[grep(10.4, rarefacts$replicate)] <- 9.8
rarefacts$Depth[grep("HC", rarefacts$replicate)] <- 9.3

Films <- c("10.4_A_4", "10.4_B_4", "10.4_C_4")
Tops <- c("10.4_A_1_A", "10.4_A_2_A", "10.4_A_3_A", "10.4_B_1_A", "10.4_B_2_A", "10.4_B_3_A", "10.4_C_1_A", "10.4_C_2_A", "10.4_C_3_A", "9.4_A_3A", "9.4_A_3B", "9.4_A_3C", "9.4_A_3D", "9.4_B_3A", "9.4_B_3B", "9.4_B_3C", "9.4_B_3C_2", "9.4_B_3C", "9.4_C_3A", "9.4_C_3B", "HC_A_1A", "HC_A_2A", "HC_A_3A", "HC_A_4A", "HC_A_5A", "HC_A_6A", "HC_B_1A", "HC_B_2A", "HC_B_3A")
Middles <- c("10.4_A_1_B", "10.4_A_2_B", "10.4_A_3_B", "10.4_B_1_B", "10.4_B_2_B", "10.4_B_3_B", "10.4_C_1_B", "10.4_C_2_B", "10.4_C_3_B", "9.4_A_4A", "9.4_A_4B", "9.4_A_4C", "9.4_A_4D", "9.4_B_4A", "9.4_B_4B", "9.4_B_4C", "9.4_C_4A", "9.4_C_4B", "9.4_C_4C", "HC_A_1B", "HC_A_2B", "HC_A_3B", "HC_A_4B", "HC_A_5B", "HC_A_6B", "HC_B_1B", "HC_B_2B", "HC_B_3B")
Bottoms <- c("10.4_A_1_C", "10.4_A_2_C", "10.4_A_3_C", "10.4_B_1_C", "10.4_B_2_C", "10.4_B_3_C", "10.4_C_1_C", "10.4_C_2_C", "10.4_C_3_C", "9.4_A_5A", "9.4_A_5B", "9.4_A_5C", "9.4_A_5D", "9.4_B_5A", "9.4_B_5B", "9.4_B_5C", "9.4_C_5A", "9.4_C_5B", "9.4_C_5C", "HC_A_1C", "HC_A_2C", "HC_A_3C", "HC_A_4C", "HC_A_5C", "HC_A_6C", "HC_B_1C", "HC_B_2C", "HC_B_3C")

rarefacts$Layer[which(is.na(pmatch(rarefacts$replicate, Films, duplicates.ok = TRUE)) == FALSE)] <- "Film"
rarefacts$Layer[which(is.na(pmatch(rarefacts$replicate, Tops, duplicates.ok = TRUE)) == FALSE)] <- "Top"
rarefacts$Layer[which(is.na(pmatch(rarefacts$replicate, Middles, duplicates.ok = TRUE)) == FALSE)] <- "Middle"
rarefacts$Layer[which(is.na(pmatch(rarefacts$replicate, Bottoms, duplicates.ok = TRUE)) == FALSE)] <- "Bottom"

rarefacts$Sample <- paste(rarefacts$Depth, rarefacts$Layer)

rarefacts$Sample <- fct_relevel(rarefacts$Sample, c("9 Top", "9 Middle", "9 Bottom", "9.3 Top", "9.3 Middle", "9.3 Bottom", "9.8 Film", "9.8 Top", "9.8 Middle", "9.8 Bottom"))

rarefacts %>%
  ggplot(mapping = aes(x = k, y = unrooted_mean, color = Sample)) +
  geom_smooth() +
  theme_minimal() +
  ylab("Average Unrooted Phylogenetic Distance") +
  xlab("Fraction of Pqueries (k)")
