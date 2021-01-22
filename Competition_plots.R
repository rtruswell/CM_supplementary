setwd("~/Dropbox/Potsdam_2019")
library(ggplot2)

Competition_data <- read.csv("CM_grammar_comparison.csv", header=T)
Competition_data$log.XYVS.100k <-log(Competition_data$mat.XYVS.100k.words+1)
Competition_data$log.subXVS.100k <- log(Competition_data$sub.XVS.100k.Words+1)
Competition_data$log.XYVS.XVS.100k <- log(Competition_data$mat.XYVS.sub.XVS.100k+1)
Competition_data$log.matXVS.100k <- log(Competition_data$Mat.XVS.pron.100k+1)

#These three aren't normally distributed
hist(Competition_data$log.XYVS.100k)
qqnorm(Competition_data$log.XYVS.100k)
qqline(Competition_data$log.XYVS.100k)
shapiro.test(Competition_data$log.XYVS.100k)

hist(Competition_data$log.subXVS.100k)
qqnorm(Competition_data$log.subXVS.100k)
qqline(Competition_data$log.subXVS.100k)
shapiro.test(Competition_data$log.subXVS.100k)

hist(Competition_data$log.XYVS.XVS.100k)
qqnorm(Competition_data$log.XYVS.XVS.100k)
qqline(Competition_data$log.XYVS.XVS.100k)
shapiro.test(Competition_data$log.XYVS.XVS.100k)

#This one, p>0.05 but only just, so let's assume nothing is normally distributed
hist(Competition_data$log.matXVS.100k)
qqnorm(Competition_data$log.matXVS.100k)
qqline(Competition_data$log.matXVS.100k)
shapiro.test(Competition_data$log.matXVS.100k)

# Matrix XYVS correlates with embedded XVS
cor.test(jitter(Competition_data$log.XYVS.100k),jitter(Competition_data$log.subXVS.100k),method = "spearman")

XYVS_subXVS.gg <- ggplot(Competition_data,aes(x=log.XYVS.100k,y=log.subXVS.100k)) +
  geom_point(aes(size=Words)) + 
  xlab("log Matrix XYVS/100k words") +
  ylab("log Embedded XVS/100k words") +
  theme_minimal()

# (Matrix XYVS + embedded XVS) correlates with matrix XVS
cor.test(jitter(Competition_data$log.XYVS.XVS.100k),jitter(Competition_data$log.matXVS.100k),method = "spearman")

CM_matXVS.gg <- ggplot(Competition_data,aes(x=log.XYVS.XVS.100k,y=log.matXVS.100k)) + 
  geom_point(aes(size=Words,color=ifelse(Text %in% c("edincmat","edincmbt","edincmct","genexodt","cmorm","cmbenrul","cmrolltr","cmrollep","cmedthor"),"blue","black"))) + 
  xlab("log (Matrix XYVS + embedded XVS)/100k words") +
  ylab("log Matrix XVS/100k words") +
  scale_color_identity() +
  theme_minimal()

CM_matXVS_OEME.gg <- ggplot(Competition_data,aes(x=log.XYVS.XVS.100k,y=log.matXVS.100k)) + 
  geom_point(aes(size=Words,color=ifelse(Year < 1151,"red","blue"))) + 
  xlab("log (Matrix XYVS + embedded XVS)/100k words") +
  ylab("log Matrix XVS/100k words") +
  scale_color_identity() +
  theme_minimal()

OE <- subset(Competition_data,Year < 1151)
OENoZero <- subset(OE,log.XYVS.100k > 0 & log.subXVS.100k > 0)
ME <- subset(Competition_data,Year > 1150)

#### NOW OE ONLY
#### NOTHING CORRELATES WITH ANYTHING, EVEN IF YOU EXCLUDE ZEROS
# Matrix XYVS correlates with embedded XVS
cor.test(jitter(OE$log.XYVS.100k),jitter(OE$log.subXVS.100k),method = "spearman")
cor.test(jitter(OENoZero$log.XYVS.100k),jitter(OENoZero$log.subXVS.100k),method = "spearman")

XYVS_subXVS_OE.gg <- ggplot(OE,aes(x=log.XYVS.100k,y=log.subXVS.100k)) +
  geom_point(aes(size=Words)) + 
  xlab("log Matrix XYVS/100k words") +
  ylab("log Embedded XVS/100k words") +
  theme_minimal()

XYVS_subXVS_OE_NoZero.gg <- ggplot(OENoZero,aes(x=log.XYVS.100k,y=log.subXVS.100k)) +
  geom_point(aes(size=Words)) + 
  xlab("log Matrix XYVS/100k words") +
  ylab("log Embedded XVS/100k words") +
  theme_minimal()

# (Matrix XYVS + embedded XVS) correlates with matrix XVS
cor.test(jitter(OE$log.XYVS.XVS.100k),jitter(OE$log.matXVS.100k),method = "spearman")
cor.test(jitter(OENoZero$log.XYVS.XVS.100k),jitter(OENoZero$log.matXVS.100k),method = "spearman")

CM_matXVS_OE.gg <- ggplot(OE,aes(x=log.XYVS.XVS.100k,y=log.matXVS.100k)) + 
  geom_point(aes(size=Words)) + 
  xlab("log (Matrix XYVS + embedded XVS)/100k words") +
  ylab("log Matrix XVS/100k words") +
  theme_minimal()

CM_matXVS_OE_NoZero.gg <- ggplot(OENoZero,aes(x=log.XYVS.XVS.100k,y=log.matXVS.100k)) + 
  geom_point(aes(size=Words)) + 
  xlab("log (Matrix XYVS + embedded XVS)/100k words") +
  ylab("log Matrix XVS/100k words") +
  theme_minimal()

#### NOW ME ONLY
cor.test(jitter(ME$log.XYVS.100k),jitter(ME$log.subXVS.100k),method = "spearman")

XYVS_subXVS_ME.gg <- ggplot(ME,aes(x=log.XYVS.100k,y=log.subXVS.100k)) +
  geom_point(aes(size=Words)) + 
  xlab("log Matrix XYVS/100k words") +
  ylab("log Embedded XVS/100k words") +
  theme_minimal()

# (Matrix XYVS + embedded XVS) correlates with matrix XVS
cor.test(jitter(ME$log.XYVS.XVS.100k),jitter(ME$log.matXVS.100k),method = "spearman")

CM_matXVS_ME.gg <- ggplot(ME,aes(x=log.XYVS.XVS.100k,y=log.matXVS.100k)) + 
  geom_point(aes(size=Words)) + 
  xlab("log (Matrix XYVS + embedded XVS)/100k words") +
  ylab("log Matrix XVS/100k words") +
  theme_minimal()

######################################################

#### Loss of V2

LateME <- subset(Competition_data,Year > 1250)
LateME$InvRate_fullNP <- LateME$PPVS.fullNP/(LateME$PPVS.fullNP+LateME$PPSV.fullNP)
LateME$InvRate_all <- (LateME$PPVS.fullNP + LateME$PPVS.pronoun)/(LateME$PPVS.fullNP+LateME$PPSV.fullNP+LateME$PPVS.pronoun+LateME$PPSV.pronoun)

# Both normally distributed, so not log-transforming
#I'm going to use full NPs only.  Because pronouns basically never invert.

cor.test(jitter(LateME$InvRate_fullNP),jitter(LateME$log.XYVS.XVS.100k),method="spearman")
LossOfV2.gg <- ggplot(LateME,aes(x=InvRate_fullNP,y=log.XYVS.XVS.100k)) +
  geom_point(aes(size=Words,col = ifelse(Text %in% c("edincmat","edincmbt","edincmct","cmbenrul","genexodt","cmrolltr","cmrollep","cmorm","cmedthor"),"blue","black"))) +
  scale_color_identity() +
  xlab("Rate of verb-second with initial PP + full NP subject") +
  ylab("log (Matrix XYVS + embedded XVS)/100k words") +
  theme_minimal()

# Spit out pdfs
pdf("XYVS_subXVS.pdf",width = 8,height = 5)
XYVS_subXVS.gg
dev.off()

pdf("CM_matXVS.pdf",width = 8,height = 5)
CM_matXVS.gg
dev.off()

pdf("XYVS_subXVS_OE.pdf",width = 8,height = 5)
XYVS_subXVS_OE.gg
dev.off()

pdf("CM_matXVS_OE.pdf",width = 8,height = 5)
CM_matXVS_OE.gg
dev.off()

pdf("CM_matXVS_OEME.pdf",width = 8,height = 5)
CM_matXVS_OEME.gg
dev.off()

pdf("XYVS_subXVS_OE_NoZero.pdf",width = 8,height = 5)
XYVS_subXVS_OE.gg
dev.off()

pdf("CM_matXVS_OE_NoZero.pdf",width = 8,height = 5)
CM_matXVS_OE.gg
dev.off()

pdf("XYVS_subXVS_ME.pdf",width = 8,height = 5)
XYVS_subXVS_ME.gg
dev.off()

pdf("CM_matXVS_ME.pdf",width = 8,height = 5)
CM_matXVS_ME.gg
dev.off()

pdf("LossOfV2.pdf", width = 8, height = 5)
LossOfV2.gg
dev.off()


