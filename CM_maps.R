setwd("~/ownCloud/Finished_talks/DAAD_mapping/") #Modified for new location
MyColours <- c("#000000","#CC0066","#7FCC55","#B2FF44") # Added this
library(rnrfa)
library(maptools)
library(raster)
library(dplyr)
library(ggplot2)
library(rgdal)
library(lme4)
library(lmerTest)

options(tibble.print_max=100)

# Replicate Kroch and Taylor 1997
PPCME2_V2 <- read.csv("V2_PPCME2.cod.ooo",sep=":",header=F)
colnames(PPCME2_V2) <- c("EarlyXP", "Inv", "SbjType", "ClauseType", "ID")
IDToFile <- read.csv("~/Dropbox/Penn_match_IDs_to_files.csv",header=T)
BasicInfo <- read.csv("~/Dropbox/Allfiles_basic_info.csv",header=T)
PPCME2_V2 <- merge(PPCME2_V2,IDToFile,by="ID")
PPCME2_V2 <- droplevels(merge(PPCME2_V2,BasicInfo,by="File"))
PPCME2_South_Mat <- subset(PPCME2_V2,File %in% c("cmtrinit-mx1","cmlambx1-mx1","cmlamb1-m1","cmsawles-m1","cmhali-m1","cmvices1-m1","cmkathe-m1","cmancriw-1-m1","cmancriw-2-m1","cmjulia-m1","cmmarga-m1") & ClauseType == "Mat" & EarlyXP %in% c("EarlyNP","EarlyPP","EarlyADVP","EarlyAP","EarlyThen","EarlyNow") & Inv %in% c("Inv","NoInv") & SbjType %in% c("FullNP","Pronoun"))
PPCME2_Ayenbite_Mat <-subset(PPCME2_V2,File == "cmayenbi-m2" & ClauseType == "Mat" & EarlyXP %in% c("EarlyNP","EarlyPP","EarlyAP","EarlyADVP","EarlyThen","EarlyNow") & Inv %in% c("Inv","NoInv") & SbjType %in% c("FullNP","Pronoun"))
PPCME2_BenRul_Mat <-subset(PPCME2_V2,File == "cmbenrul-m3" & ClauseType == "Mat" & EarlyXP %in% c("EarlyNP","EarlyPP","EarlyAP","EarlyADVP","EarlyThen","EarlyNow") & Inv %in% c("Inv","NoInv") & SbjType %in% c("FullNP","Pronoun"))
PPCME2_South_Mat %>% group_by(EarlyXP,SbjType,Inv) %>% summarize(Total = n()) -> PPCME2_South_Mat_Counts
PPCME2_Ayenbite_Mat %>% group_by(EarlyXP,SbjType,Inv) %>% summarize(Total = n()) -> PPCME2_Ayenbite_Mat_Counts
PPCME2_BenRul_Mat %>% group_by(EarlyXP,SbjType,Inv) %>% summarize(Total = n()) -> PPCME2_BenRul_Mat_Counts
# Then enter counts from there into table in LaTeX

# Verse vs. prose
PCMEP_V2 <- read.csv("V2_PCMEP.cod.ooo",sep=":",header=F)
colnames(PCMEP_V2) <- c("EarlyXP", "Inv", "SbjType", "ClauseType", "ID")
PCMEP_V2 <- merge(PCMEP_V2,IDToFile,by="ID")
PCMEP_V2 <- droplevels(merge(PCMEP_V2,BasicInfo,by="File"))
PPCME2_Prose <- subset(PPCME2_V2,File != "cmorm-m1")
PPCME2_Verse <- subset(PPCME2_V2,File == "cmorm-m1")
PPCME2_Prose$Genre <- "Prose"
PPCME2_Verse$Genre <- "Verse"
PCMEP_V2$Genre <- "Verse"
AllPenn <- rbind(PPCME2_Prose,PPCME2_Verse)
AllPenn <- rbind(AllPenn,PCMEP_V2)
AllPenn_Useable <- subset(AllPenn,ClauseType %in% c("Mat","Sub") & EarlyXP %in% c("EarlyNP", "EarlyPP", "EarlyAP", "EarlyThen", "EarlyNow", "EarlyADVP") & SbjType %in% c("Pronoun","FullNP") & Inv %in% c("Inv","NoInv"))
AllPenn_MatrixV2 <- subset(AllPenn_Useable,ClauseType == "Mat")
AllPenn_EmbV2 <- subset(AllPenn_Useable,ClauseType == "Sub")
AllPenn_PronV2 <- subset(AllPenn_Useable,ClauseType == "Mat" & SbjType == "Pronoun")
AllPenn_FullV2 <- subset(AllPenn_Useable,ClauseType == "Mat" & SbjType == "FullNP")
# Not sure about best model structure here.
AllPenn.glmer <- lmer(ifelse(Inv == "Inv",1,0) ~ ClauseType + SbjType + Year + Genre + (1|File), data = AllPenn_Useable)
# Then summary

# Load basic data and get coordinates for mapping
PLAEME_data <- read.csv("PLAEME_more_info.csv",header=T)
PLAEME_data$Filename <- tolower(PLAEME_data$Filename)
Localized <- subset(PLAEME_data,New.grid.ref != "ZZ000000")
Points = osg_parse(Localized$New.grid.ref,coord_system = "WGS84") #New syntax for coord_system
Localized$lat <- Points$lat
Localized$lon <- Points$lon

setwd("~/Dropbox/Potsdam_2019/") #Want to print maps in Potsdam folder
latmin=49.9
latmax=55.8
lonmin=-6
lonmax=2
Localized %>% group_by(lat,lon,Approx.year) %>% summarize(TotalWords=sum(Words)) -> MapPoints

UK1<-getData("GADM", country="gb", level=1)
England <- UK1[UK1@data$NAME_1=="England",]

theme_opts<-list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_blank()))


England.gg <- ggplot() + 
  geom_path(fortify(England),mapping=aes(x=long,y=lat,group=group,lwd=.1)) +
  theme(aspect.ratio=1) + 
  theme_opts

V2data <-read.csv("~/ownCloud/Finished_talks/DAAD_mapping/V2.cod.ooo", sep=":", header=F)
colnames(V2data) <- c("EarlyXP", "Inv", "SbjType", "ClauseType", "Filename")
V2data$Filename <- tolower(V2data$Filename)
UseableV2data <- subset(V2data,ClauseType %in% c("Mat","Sub") & EarlyXP %in% c("EarlyNP", "EarlyPP", "EarlyAP", "EarlyThen", "EarlyNow", "EarlyADVP") & SbjType %in% c("Pronoun","FullNP") & Inv %in% c("Inv","NoInv"))
MatrixV2data <- subset(UseableV2data,ClauseType == "Mat")
MatrixV2data <- droplevels(MatrixV2data)
LocalMatrixV2data <- subset(MatrixV2data, Filename %in% Localized$Filename)
LocalMatrixV2data <- merge(LocalMatrixV2data,Localized,by="Filename")
V2 <- subset(LocalMatrixV2data,Inv == "Inv")
NotV2 <- subset(LocalMatrixV2data, Inv == "NoInv")
V2 %>% group_by(lat,lon,Words) %>% summarize(V2 = n()) -> V2counts
NotV2 %>% group_by(lat,lon,Words) %>% summarize(NotV2 = n()) -> NotV2counts
V2OrNot <- merge(V2counts,NotV2counts,all=T)
V2OrNot[is.na(V2OrNot)] <- 0
#V2.gg <- England.gg +
#  geom_point(data = V2OrNot,aes(y=lat,x=lon,size=V2+NotV2,color=V2/(V2+NotV2))) + 
#  # scale_color_continuous(name="% V2") +
#  scale_color_gradientn(colours=MyColours,name="% V2") +
#  scale_size_area(name = "Tokens") +
#  theme_void()
#pdf("V2_BW.pdf",height=4,width=4)
#V2.gg
#dev.off()
#V2 %>% group_by(Approx.year,Filename,Words) %>% summarize(V2=n()) -> V2Yearcounts
#NotV2 %>% group_by(Approx.year,Filename,Words) %>% summarize(NotV2=n()) -> NotV2Yearcounts
#V2ByYear <- merge(V2Yearcounts,NotV2Yearcounts,all=T)
#V2ByYear[is.na(V2ByYear)] <- 0
#V2ByYear.gg <- ggplot(V2ByYear,aes(x=Approx.year,y=V2/(V2+NotV2))) +
#  geom_point(aes(size=V2+NotV2,col=ifelse(Filename %in% c("edincmat","edincmbt","edincmct"),"red","black"))) +
#  geom_smooth(data=LocalMatrixV2data,aes(x=Approx.year,y=ifelse(Inv=="Inv",1,0))) +
#  scale_size_area(name = "Tokens") +
#  scale_color_identity() +
#  xlab("Year") +
#  ylab("% V2") +
#  theme_minimal()
#pdf("V2ByYear_BW.pdf",height=4,width=8)
#V2ByYear.gg
#dev.off()
#V2.glm <- glm(ifelse(Inv == "Inv",1,0) ~ lat * lon + Approx.year, data = LocalMatrixV2data)
#V2.glmer <- lmer(ifelse(Inv == "Inv",1,0) ~ lat * lon + Approx.year + (1|Filename), data = LocalMatrixV2data)
#V2.glmer2 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat * lon + (1|Filename), data = LocalMatrixV2data)
#V2.glmer3 <- lmer(ifelse(Inv == "Inv",1,0) ~ (1|Filename), data = LocalMatrixV2data)
# ANOVA prefers glmer2: nonsignificantly better than 1, and significantly better than 3. So lat * lon is significant predictor

# What about embedded V2?
#EmbV2data <- subset(UseableV2data,ClauseType == "Sub")
#EmbV2data <- droplevels(EmbV2data)
#LocalEmbV2data <- subset(EmbV2data, Filename %in% Localized$Filename)
#LocalEmbV2data <- merge(LocalEmbV2data,Localized,by="Filename")
#EmbV2 <- subset(LocalEmbV2data,Inv == "Inv")
#NotEmbV2 <- subset(LocalEmbV2data, Inv == "NoInv")
#EmbV2 %>% group_by(lat,lon,Words) %>% summarize(V2 = n()) -> EmbV2counts
#NotEmbV2 %>% group_by(lat,lon,Words) %>% summarize(NotV2 = n()) -> NotEmbV2counts
#EmbV2OrNot <- merge(EmbV2counts,NotEmbV2counts,all=T)
#EmbV2OrNot[is.na(EmbV2OrNot)] <- 0
#EmbV2.gg <- ggmap(England) +
#  geom_point(data = EmbV2OrNot,aes(y=lat,x=lon,size=V2+NotV2,color=V2/(V2+NotV2))) + 
#  #scale_color_continuous() +
#  scale_color_gradientn(colours=MyColours) +
#  theme_void()
#EmbV2.glm <- glm(ifelse(Inv == "Inv",1,0) ~ lat * lon + Approx.year, data = LocalEmbV2data)
#EmbV2.glmer <- lmer(ifelse(Inv == "Inv",1,0) ~ lat * lon + Approx.year + (1|Filename), data = LocalEmbV2data)
#EmbV2.glmer2 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + lon + Approx.year + (1|Filename), data = LocalEmbV2data)
#EmbV2.glmer3 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + Approx.year + (1|Filename), data = LocalEmbV2data)
#EmbV2.glmer4 <- lmer(ifelse(Inv == "Inv",1,0) ~ Approx.year + (1|Filename), data = LocalEmbV2data)
#EmbV2.glmer5 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + (1|Filename), data = LocalEmbV2data)
#EmbV2.glmer6 <- lmer(ifelse(Inv == "Inv",1,0) ~ (1|Filename), data = LocalEmbV2data)
## glmer3 preferred: significantly better than 4, 5, 6, nonsignificantly better than 2, 3.
## Model with year alone worse than null model but better than model with just lat.

# Matrix V2 with pronominal subjects

MatPronV2data <- subset(UseableV2data,ClauseType == "Mat" & SbjType == "Pronoun")
MatPronV2data <- droplevels(MatPronV2data)
LocalMatPronV2data <- subset(MatPronV2data, Filename %in% Localized$Filename)
LocalMatPronV2data <- merge(LocalMatPronV2data,Localized,by="Filename")
MatPronV2 <- subset(LocalMatPronV2data,Inv == "Inv")
NotMatPronV2 <- subset(LocalMatPronV2data, Inv == "NoInv")
MatPronV2 %>% group_by(lat,lon,Words) %>% summarize(V2 = n()) -> MatPronV2counts
NotMatPronV2 %>% group_by(lat,lon,Words) %>% summarize(NotV2 = n()) -> NotMatPronV2counts
MatPronV2OrNot <- merge(MatPronV2counts,NotMatPronV2counts,all=T)
MatPronV2OrNot[is.na(MatPronV2OrNot)] <- 0
MatPronV2.gg <- England.gg +
  geom_point(data = MatPronV2OrNot,aes(y=lat,x=lon,size=V2+NotV2,color=V2/(V2+NotV2))) + 
  #scale_color_continuous(name= "% V2") +
  scale_color_gradientn(colours=MyColours,name="% V2") +
  scale_size_area(name = "Tokens") +
  theme_void()
pdf("MatPronV2_BW.pdf",height=4,width=4)
MatPronV2.gg
dev.off()
#MatPronV2.glm <- glm(ifelse(Inv == "Inv",1,0) ~ lat * lon + Approx.year, data = LocalMatPronV2data)
#MatPronV2.glmer <- lmer(ifelse(Inv == "Inv",1,0) ~ lat * lon + Approx.year + (1|Filename), data = LocalMatPronV2data)
#MatPronV2.glmer2 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + lon + Approx.year + (1|Filename), data = LocalMatPronV2data)
#MatPronV2.glmer3 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat * lon + (1|Filename), data = LocalMatPronV2data)
#MatPronV2.glmer4 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + lon + (1|Filename), data = LocalMatPronV2data)
#MatPronV2.glmer5 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + (1|Filename), data = LocalMatPronV2data)
#MatPronV2.glmer6 <- lmer(ifelse(Inv == "Inv",1,0) ~ (1|Filename), data = LocalMatPronV2data)
# ANOVA prefers glmer3. 5 is close.  So lat is definitely significant predictor, lon + interaction marginally

# Matrix V2 with full NP subjects

MatFullV2data <- subset(UseableV2data,ClauseType == "Mat" & SbjType == "FullNP")
MatFullV2data <- droplevels(MatFullV2data)
LocalMatFullV2data <- subset(MatFullV2data, Filename %in% Localized$Filename)
LocalMatFullV2data <- merge(LocalMatFullV2data,Localized,by="Filename")
MatFullV2 <- subset(LocalMatFullV2data,Inv == "Inv")
NotMatFullV2 <- subset(LocalMatFullV2data, Inv == "NoInv")
MatFullV2 %>% group_by(lat,lon,Words) %>% summarize(V2 = n()) -> MatFullV2counts
NotMatFullV2 %>% group_by(lat,lon,Words) %>% summarize(NotV2 = n()) -> NotMatFullV2counts
MatFullV2OrNot <- merge(MatFullV2counts,NotMatFullV2counts,all=T)
MatFullV2OrNot[is.na(MatFullV2OrNot)] <- 0
MatFullV2.gg <- England.gg +
  geom_point(data = MatFullV2OrNot,aes(y=lat,x=lon,size=V2+NotV2,color=V2/(V2+NotV2))) + 
  #scale_color_continuous(name= "% V2") +
  scale_color_gradientn(colours=MyColours,name="% V2") +
  scale_size_area(name = "Tokens") +
  theme_void()
pdf("MatFullV2_BW.pdf",height=4,width=4)
MatFullV2.gg
dev.off()
#MatFullV2.glm <- glm(ifelse(Inv == "Inv",1,0) ~ lat * lon + Approx.year, data = LocalMatFullV2data)
#MatFullV2.glmer <- lmer(ifelse(Inv == "Inv",1,0) ~ lat * lon + Approx.year + (1|Filename), data = LocalMatFullV2data)
#MatFullV2.glmer2 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat * lon + (1|Filename), data = LocalMatFullV2data)
#MatFullV2.glmer3 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + lon + (1|Filename), data = LocalMatFullV2data)
#MatFullV2.glmer4 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + (1|Filename), data = LocalMatFullV2data)
#MatFullV2.glmer5 <- lmer(ifelse(Inv == "Inv",1,0) ~ Approx.year + (1|Filename), data = LocalMatFullV2data)
#MatFullV2.glmer6 <- lmer(ifelse(Inv == "Inv",1,0) ~ (1|Filename), data = LocalMatFullV2data)
# Embedded V2 with pronominal subjects

EmbPronV2data <- subset(UseableV2data,ClauseType == "Sub" & SbjType == "Pronoun")
EmbPronV2data <- droplevels(EmbPronV2data)
LocalEmbPronV2data <- subset(EmbPronV2data, Filename %in% Localized$Filename)
LocalEmbPronV2data <- merge(LocalEmbPronV2data,Localized,by="Filename")
EmbPronV2 <- subset(LocalEmbPronV2data,Inv == "Inv")
NotEmbPronV2 <- subset(LocalEmbPronV2data, Inv == "NoInv")
EmbPronV2 %>% group_by(lat,lon,Words) %>% summarize(V2 = n()) -> EmbPronV2counts
NotEmbPronV2 %>% group_by(lat,lon,Words) %>% summarize(NotV2 = n()) -> NotEmbPronV2counts
EmbPronV2OrNot <- merge(EmbPronV2counts,NotEmbPronV2counts,all=T)
EmbPronV2OrNot[is.na(EmbPronV2OrNot)] <- 0
EmbPronV2.gg <- ggmap(England) +
  geom_point(data = EmbPronV2OrNot,aes(y=lat,x=lon,size=V2+NotV2,color=V2/(V2+NotV2))) + 
  #scale_color_continuous(name= "% V2") +
  scale_color_gradientn(colours=MyColours,name="% V2") +
  scale_size_area(name = "Tokens") +
  theme_void()
pdf("EmbPronV2_BW.pdf",height = 4,width =4)
EmbPronV2.gg
dev.off()
#EmbPronV2.glm <- glm(ifelse(Inv == "Inv",1,0) ~ lat * lon + Approx.year, data = LocalEmbPronV2data)
#%EmbPronV2.glmer <- lmer(ifelse(Inv == "Inv",1,0) ~ lat * lon + Approx.year + (1|Filename), data = LocalEmbPronV2data)
#%EmbPronV2.glmer2 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + lon + Approx.year + (1|Filename), data = LocalEmbPronV2data)
#%EmbPronV2.glmer3 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat * lon + (1|Filename), data = LocalEmbPronV2data)
#%EmbPronV2.glmer4 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + lon + (1|Filename), data = LocalEmbPronV2data)
#%EmbPronV2.glmer5 <- lmer(ifelse(Inv == "Inv",1,0) ~ lat + (1|Filename), data = LocalEmbPronV2data)
#EmbPronV2.glmer6 <- lmer(ifelse(Inv == "Inv",1,0) ~ (1|Filename), data = LocalEmbPronV2data)
# 5 best if anything so some effect of lat but much flakier than the other results.
