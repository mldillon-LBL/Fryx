# Want to test beta diversity 
library(tidyverse)

dat <- read_tsv(file = "kr_matrix.tsv")
kr_rep.matrix <- data.matrix(dat[2:length(dat)])
row.names(kr_rep.matrix) <- dat$`Z_1 distances:`
kr_rep.matrix[is.na(kr_rep.matrix)] <- 0

kr_rep.matrix <- kr_rep.matrix + t(kr_rep.matrix)
kr_rep.df <- as.data.frame(kr_rep.matrix)

#Need a metadata dataframe to compare with.

Reps_meta <- read_tsv(file = "Reps_meta.tsv")
reps_meta.matrix <- data.matrix(Reps_meta[2:length(Reps_meta)])
row.names(reps_meta.matrix) <- Reps_meta$Rep

reps_meta.matrix <- reps_meta.matrix[rownames(kr_rep.matrix), , drop = FALSE]
reps_meta.df <- as.data.frame(reps_meta.matrix)

library(vegan)          

adonis(kr_rep.df ~ Layer_num*Depth, data = reps_meta.df)
adonis(kr_rep.df ~ Layer_num, data = reps_meta.df)
adonis(kr_rep.df ~ Depth, data = reps_meta.df)
adonis(kr_rep.df ~ DO_mg_L, data = reps_meta.df)

perm <- how(nperm = 199)
setBlocks(perm) <- with(reps_meta.df, Depth)
adonis2(kr_rep.df ~ Layer_num + Depth + EC_mS_cm + DO_mg_L + pH + DIC_mg_L + DRP_µg_L, data = reps_meta.df, permutations = perm)
adonis2(kr_rep.df ~ Layer_num, data = reps_meta.df, permutations = perm)
adonis2(kr_rep.df ~ Layer_num*Depth, data = reps_meta.df, permutations = perm)

lf_cap <- capscale(kr_rep.df ~ Layer_num + Depth + EC_mS_cm + DO_mg_L + pH + DIC_mg_L + DRP_µg_L, data = reps_meta.df, permutations = perm)

lf_cap <- capscale(kr_rep.df ~ Layer_num*Depth, data = reps_meta.df)

#also need to know about irradiance, so make a subset that has irradiance values

dat_irr <- read_tsv(file = "kr_matrix_irr.tsv")
kr_rep_irr.matrix <- data.matrix(dat_irr[2:length(dat_irr)])
row.names(kr_rep_irr.matrix) <- dat_irr$`Z_1 distances:`
kr_rep_irr.matrix[is.na(kr_rep_irr.matrix)] <- 0

kr_rep_irr.matrix <- kr_rep_irr.matrix + t(kr_rep_irr.matrix)
kr_rep_irr.df <- as.data.frame(kr_rep_irr.matrix)

#Need a metadata dataframe to compare with.

reps_meta.irr <- read_tsv(file = "Reps_meta_irr.tsv")
reps_meta_irr.matrix <- data.matrix(reps_meta.irr[2:length(reps_meta.irr)])
row.names(reps_meta_irr.matrix) <- reps_meta.irr$Rep

reps_meta_irr.matrix <- reps_meta_irr.matrix[rownames(kr_rep_irr.matrix), , drop = FALSE]
reps_meta.irr.df <- as.data.frame(reps_meta_irr.matrix)

adonis(kr_rep_irr.df ~ `Irradiance_%_surface`, data = reps_meta.irr.df)

adonis(kr_rep_irr.df ~ `Irradiance_%_surface`*Depth, data = reps_meta.irr.df)
perm.irr <- how(nperm = 199)
setBlocks(perm.irr) <- with(reps_meta.irr.df, Depth)
adonis2(kr_rep_irr.df ~ `Irradiance_%_surface`*DO_mg_L, data = reps_meta.irr.df, permutations = perm.irr)

#Ah, so within depth, irradiance is significant.

lf.irr_cap <- capscale(kr_rep_irr.df ~ `Irradiance_%_surface`*DO_mg_L + Layer_num*Depth, data = reps_meta.irr.df, permutations = perm.irr)

plot(lf.irr_cap, pcol = "gray", pch = "+")
#What this plot is saying is that for the replicates for which we have irradiance and DO data, Depth is a proxy for DO where greater DO = less depth and layer is a proxy for irradiance where greater irradiance = deeper layer.  This should be in supplement and we should use the proxies in the actual paper.

