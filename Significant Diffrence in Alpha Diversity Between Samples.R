
#Abundance-Weighted Phylogentic Distance: Difference between groups?
#bwpd is guppy output from fpd

bwpd <- read.delim("~/Documents/Fryxell/Diversity as Seasonal to Constant/Alpha Diversity/bwpd.txt")
#bwpd$Layer <- factor(bwpd$Layer, levels = c("Bottom", "Middle", "Top", "Ridge", "Knob", "Film"))
#bwpd$Depth <- sort(bwpd$Depth, decreasing = FALSE)

bwpd.lm <- lm(bwpd ~ Depth*Layer, data = bwpd)
bwpd.anova <- anova(bwpd.lm)
summary(bwpd.lm)
bwpd.anova
bwpd.aov <- aov(bwpd ~ as.factor(Depth)*Layer, data = bwpd)

bwpd.tukey <- TukeyHSD(x = bwpd.aov, conf.level = 0.95)
bwpd.tukey

bwpd.boxplot <- boxplot(bwpd ~ Layer*Depth, data = bwpd, horizontal = TRUE, ylim = c(0, 3.5), notch = TRUE, las = 1, frame = FALSE, yaxt = "n")
#axis(side = 2, at = seq(1:18), labels = c("9.8 Bottom", "9.8 Middle", "9.8 Top", "", "", "9.8 Film","9.35 m Bottom", "9.35 m Middle","9.35 m Top","", "", "", "9.0 m Bottom", "9.0 m Middle", "9.0 m Top", "9.0 m Ridge", "9.0 m Knob", ""), las = 1)
pdf("bwpd.boxplot.pdf")

#Abundance-Weighted Phylogentic Distance: Difference between depths?
#bwpd is guppy output from fpd

bwpd.d.lm <- lm(bwpd ~ Depth, data = bwpd)
bwpd.d.anova <- anova(bwpd.d.lm)
summary(bwpd.d.lm)
bwpd.d.anova
bwpd.d.aov <- aov(bwpd ~ as.factor(Depth), data = bwpd)

bwpd.d.tukey <- TukeyHSD(x = bwpd.d.aov, conf.level = 0.95)
bwpd.d.tukey

bwpd.d.boxplot <- boxplot(bwpd ~ Depth, data = bwpd, horizontal = TRUE, ylim = c(0, 3.5), notch = TRUE, las = 1, frame = "n", main = "Alpha Diversity of All Layers by Depth", xlab = "Phylogenetic Alpha Diversity (bwpd)")
pdf("bwpd.d.boxplot.pdf")

#Abundance-Weighted Phylogentic Distance: Difference between layers?
#bwpd is guppy output from fpd

bwpd$Layer2 <- bwpd$Layer
bwpd$Layer2 <- gsub(pattern = "Knob", replacement = "Top", x = bwpd$Layer2)
bwpd$Layer2 <- gsub(pattern = "Ridge", replacement = "Top", x = bwpd$Layer2)
bwpd$Layer2 <- gsub(pattern = "Film", replacement = "Top", x = bwpd$Layer2)

bwpd.l.lm <- lm(bwpd ~ Layer, data = bwpd)
bwpd.l.anova <- anova(bwpd.l.lm)
summary(bwpd.l.lm)
bwpd.l.anova
bwpd.l.aov <- aov(bwpd ~ Layer, data = bwpd)

bwpd.l.tukey <- TukeyHSD(x = bwpd.l.aov, conf.level = 0.95)
bwpd.l.tukey

bwpd.l.boxplot <- boxplot(bwpd ~ Layer2, data = bwpd, horizontal = TRUE, ylim = c(0, 3.5), notch = TRUE, las = 1, frame = "n", main = "Alpha Diversity of All Depths by Layer", xlab = "Phylogenetic Alpha Diversity (bwpd)")
pdf("bwpd.l.boxplot.pdf")

#Is there a difference in phylogenetic diversity between layers just in the 9.0 m mats?

bwpd_9 <- bwpd[which(bwpd$Depth == 9.00), ]
bwpd_9$PAR <- c(rep("B", 16), rep("M", 19), rep("S", 23))

bwpd_9.lm <- lm(bwpd ~ PAR, data = bwpd_9)
bwpd_9.anova <- anova(bwpd_9.lm)
summary(bwpd_9.lm)
bwpd_9.anova
bwpd_9.aov <- aov(bwpd ~ as.factor(PAR), data = bwpd_9)

bwpd_9.tukey <- TukeyHSD(x = bwpd_9.aov, conf.level = 0.95)
bwpd_9.tukey

bwpd_9.boxplot <- boxplot(bwpd ~ PAR, data = bwpd_9, horizontal = TRUE, notch = TRUE, ylim = c(0, 3.5),las = 1, frame = FALSE, main = "Alpha Diversity of Each Layer at 9.0 m", xlab = "Phylogenetic Diversity (bwpd)")
pdf("bwpd.boxplot.pdf")

#Is there a difference in phylogenetic diversity between depths just in the surface layers?

bwpd_S <- bwpd[which(bwpd$Layer == "Knob" | bwpd$Layer == "Ridge" | bwpd$Layer == "Top" | bwpd$Layer == "Film"), ]

bwpd_S.lm <- lm(bwpd ~ Depth, data = bwpd_S)
bwpd_S.anova <- anova(bwpd_S.lm)
summary(bwpd_S.lm)
bwpd_S.anova
bwpd_S.aov <- aov(bwpd ~ as.factor(Depth), data = bwpd_S)

bwpd_S.tukey <- TukeyHSD(x = bwpd_S.aov, conf.level = 0.95)
bwpd_S.tukey

bwpd_S.boxplot <- boxplot(bwpd ~ Depth, data = bwpd_S, horizontal = TRUE, notch = TRUE, ylim = c(0, 3.5), las = 1, frame = FALSE, main = "Surface Layer Alpha Diversity", xlab = "Phylogenetic Diveristy (bwpd)")

#How about the bottom layers?

bwpd_B <- bwpd[which(bwpd$Layer == "Bottom"), ]

bwpd_B.lm <- lm(bwpd ~ Depth, data = bwpd_B)
bwpd_B.anova <- anova(bwpd_B.lm)
summary(bwpd_B.lm)
bwpd_B.anova
bwpd_B.aov <- aov(bwpd ~ as.factor(Depth), data = bwpd_B)

bwpd_B.tukey <- TukeyHSD(x = bwpd_B.aov, conf.level = 0.95)
bwpd_B.tukey

bwpd_B.boxplot <- boxplot(bwpd ~ Depth, data = bwpd_B, horizontal = TRUE, notch = TRUE, ylim = c(0, 3.5), las = 1, frame = FALSE, main = "Bottom Layer Alpha Diversity", xlab = "Phylogenetic Alpha Diveristy (bwpd)")


#What if I use cyanobacteria relative abundace as an independent variable?

bwpd_C <- bwpd[1:133, ]
bwpd_C.lm <- lm(bwpd ~ Cyanos, data = bwpd_C)
bwpd_C.anova <- anova(bwpd_C.lm)
summary(bwpd_C.lm)

qplot(x = Cyanos, y = bwpd, data = bwpd, color = Layer, size = Depth)
plot(x = bwpd$Cyanos, y = bwpd$bwpd, col = bwpd$Color, las = 1)