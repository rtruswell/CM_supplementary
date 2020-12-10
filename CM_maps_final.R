MyColours <- c("#000000","#CC0066","#7FCC55","#B2FF44") # Added this
library(rnrfa)
library(maptools)
library(raster)
library(dplyr)
library(ggplot2)
library(rgdal)

options(tibble.print_max=100)

# Load metadata and get coordinates for mapping
PLAEME_data <- read.csv("PLAEME_more_info.csv",header=T)
PLAEME_data$Filename <- tolower(PLAEME_data$Filename)
Localized <- subset(PLAEME_data,New.grid.ref != "ZZ000000")
Points = osg_parse(Localized$New.grid.ref,coord_system = "WGS84") #New syntax for coord_system
Localized$lat <- Points$lat
Localized$lon <- Points$lon

#Make basic England map
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
  geom_path(fortify(England),mapping=aes(x=long,y=lat,group=group)) +
  theme(aspect.ratio=1) + 
  theme_opts

#Read corpus data and filter out unusable tokens
V2data <-read.csv("V2.cod.ooo", sep=":", header=F)
colnames(V2data) <- c("EarlyXP", "Inv", "SbjType", "ClauseType", "Filename")
V2data$Filename <- tolower(V2data$Filename)
UseableV2data <- subset(V2data,ClauseType %in% c("Mat","Sub") & EarlyXP %in% c("EarlyNP", "EarlyPP", "EarlyAP", "EarlyThen", "EarlyNow", "EarlyADVP") & SbjType %in% c("Pronoun","FullNP") & Inv %in% c("Inv","NoInv"))

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
  scale_color_gradientn(colours=MyColours,name="% V2") +
  scale_size_area(name = "Tokens") +
  theme_void()
pdf("MatPronV2_BW.pdf",height=4,width=4)
MatPronV2.gg
dev.off()

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
  scale_color_gradientn(colours=MyColours,name="% V2") +
  scale_size_area(name = "Tokens") +
  theme_void()
pdf("MatFullV2_BW.pdf",height=4,width=4)
MatFullV2.gg
dev.off()