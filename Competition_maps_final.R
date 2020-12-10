library(rnrfa)
library(ggmap)
library(dplyr)
library(raster)
library(reshape2)
library(gridExtra)
options(tibble.print_max=100)

# Load data and clean up
# Matrix IPs
MatrixIPs <- read.csv("Competition_mat_PLAEME_full_v3.cod.ooo",header=F,sep=":")
colnames(MatrixIPs) <- c("First","Second","Third","Fourth","SbjType","Ob1Type","Ob2Type","Filename")
MatrixIPs$Filename <- tolower(MatrixIPs$Filename)
MatrixIPs$SbjType[MatrixIPs$SbjType == "ProSelf"] <- "FullNP" # Treat complicated "pronoun + X" types, normally as pronouns except this case
MatrixIPs$Ob1Type[MatrixIPs$Ob1Type == "ProSelf"] <- "FullNP"
MatrixIPs$Ob2Type[MatrixIPs$Ob2Type == "ProSelf"] <- "FullNP"
MatrixIPs$SbjType[MatrixIPs$SbjType %in% c("ProPlus","ProTrace")] <- "Pronoun"
MatrixIPs$Ob1Type[MatrixIPs$Ob1Type %in% c("ProPlus","ProTrace")] <- "Pronoun"
MatrixIPs$Ob2Type[MatrixIPs$Ob2Type %in% c("ProPlus","ProTrace")] <- "Pronoun"
MatrixIPs$SbjType[MatrixIPs$SbjType == "_"] <- "FullNP" # Mainly indefinite pronouns (one, man) or expletive "there"
MatrixIPs$Ob1Type[MatrixIPs$Ob1Type == "_"] <- "FullNP"
MatrixIPs$Ob2Type[MatrixIPs$Ob2Type == "_"] <- "FullNP"

# Encode OB1 as OB1Pro when object is a pronoun
MatrixIPs %>% filter(Ob1Type == "Pronoun") -> MatrixIPsOb1Pro
MatrixIPs %>% filter(Ob1Type != "Pronoun") -> MatrixIPsOb1Full
MatrixIPsOb1Pro %>% filter(First == "OB1") -> MatrixIPsOb1ProFirst
MatrixIPsOb1Pro %>% filter(Second == "OB1") -> MatrixIPsOb1ProSecond
MatrixIPsOb1Pro %>% filter(Third == "OB1") -> MatrixIPsOb1ProThird
MatrixIPsOb1Pro %>% filter(Fourth == "OB1") -> MatrixIPsOb1ProFourth
MatrixIPsOb1Pro %>% filter(First != "OB1" & Second != "OB1" & Third != "OB1" & Fourth != "OB1") -> MatrixIPsOb1ProRest
MatrixIPsOb1ProFirst %>% mutate(First = "OB1Pro") -> MatrixIPsOb1ProFirst
MatrixIPsOb1ProSecond %>% mutate(Second = "OB1Pro") -> MatrixIPsOb1ProSecond
MatrixIPsOb1ProThird %>% mutate(Third = "OB1Pro") -> MatrixIPsOb1ProThird
MatrixIPsOb1ProFourth %>% mutate(Fourth = "OB1Pro") -> MatrixIPsOb1ProFourth
MatrixIPs <- rbind (MatrixIPsOb1Full,MatrixIPsOb1ProFirst)
MatrixIPs <- rbind (MatrixIPs,MatrixIPsOb1ProSecond)
MatrixIPs <- rbind (MatrixIPs,MatrixIPsOb1ProThird)
MatrixIPs <- rbind (MatrixIPs,MatrixIPsOb1ProFourth)
MatrixIPs <- rbind (MatrixIPs,MatrixIPsOb1ProRest)

# Encode OB2 as OB2Pro when object is a pronoun
MatrixIPs %>% filter(Ob2Type == "Pronoun") -> MatrixIPsOb2Pro
MatrixIPs %>% filter(Ob2Type != "Pronoun") -> MatrixIPsOb2Full
MatrixIPsOb2Pro %>% filter(First == "OB2") -> MatrixIPsOb2ProFirst
MatrixIPsOb2Pro %>% filter(Second == "OB2") -> MatrixIPsOb2ProSecond
MatrixIPsOb2Pro %>% filter(Third == "OB2") -> MatrixIPsOb2ProThird
MatrixIPsOb2Pro %>% filter(Fourth == "OB2") -> MatrixIPsOb2ProFourth
MatrixIPsOb2Pro %>% filter(First != "OB2" & Second != "OB2" & Third != "OB2" & Fourth != "OB2") -> MatrixIPsOb2ProRest
MatrixIPsOb2ProFirst %>% mutate(First = "OB2Pro") -> MatrixIPsOb2ProFirst
MatrixIPsOb2ProSecond %>% mutate(Second = "OB2Pro") -> MatrixIPsOb2ProSecond
MatrixIPsOb2ProThird %>% mutate(Third = "OB2Pro") -> MatrixIPsOb2ProThird
MatrixIPsOb2ProFourth %>% mutate(Fourth = "OB2Pro") -> MatrixIPsOb2ProFourth
MatrixIPs <- rbind (MatrixIPsOb2Full,MatrixIPsOb2ProFirst)
MatrixIPs <- rbind (MatrixIPs,MatrixIPsOb2ProSecond)
MatrixIPs <- rbind (MatrixIPs,MatrixIPsOb2ProThird)
MatrixIPs <- rbind (MatrixIPs,MatrixIPsOb2ProFourth)
MatrixIPs <- rbind (MatrixIPs,MatrixIPsOb2ProRest)


#Subordinate IPs (THT and ADV CPs only)
SubIPs <- read.csv("Competition_sub_PLAEME_full_v3.cod.ooo",header=F,sep=":")
colnames(SubIPs) <- c("First","Second","Third","Fourth","SbjType","Ob1Type","Ob2Type","Filename")
SubIPs$Filename <- tolower(SubIPs$Filename)
SubIPs$SbjType[SubIPs$SbjType == "ProSelf"] <- "FullNP" # As above
SubIPs$Ob1Type[SubIPs$Ob1Type == "ProSelf"] <- "FullNP"
SubIPs$Ob2Type[SubIPs$Ob2Type == "ProSelf"] <- "FullNP"
SubIPs$SbjType[SubIPs$SbjType %in% c("ProPlus","ProTrace")] <- "Pronoun"
SubIPs$Ob1Type[SubIPs$Ob1Type %in% c("ProPlus","ProTrace")] <- "Pronoun"
SubIPs$Ob2Type[SubIPs$Ob2Type %in% c("ProPlus","ProTrace")] <- "Pronoun"
SubIPs$SbjType[SubIPs$SbjType == "_"] <- "FullNP" # Mainly indefinite pronouns (one, man) or expletive "there"
SubIPs$Ob1Type[SubIPs$Ob1Type == "_"] <- "FullNP"
SubIPs$Ob2Type[SubIPs$Ob2Type == "_"] <- "FullNP"

# Encode OB1 as OB1Pro when object is a pronoun
SubIPs %>% filter(Ob1Type == "Pronoun") -> SubIPsOb1Pro
SubIPs %>% filter(Ob1Type != "Pronoun") -> SubIPsOb1Full
SubIPsOb1Pro %>% filter(First == "OB1") -> SubIPsOb1ProFirst
SubIPsOb1Pro %>% filter(Second == "OB1") -> SubIPsOb1ProSecond
SubIPsOb1Pro %>% filter(Third == "OB1") -> SubIPsOb1ProThird
SubIPsOb1Pro %>% filter(Fourth == "OB1") -> SubIPsOb1ProFourth
SubIPsOb1Pro %>% filter(First != "OB1" & Second != "OB1" & Third != "OB1" & Fourth != "OB1") -> SubIPsOb1ProRest
SubIPsOb1ProFirst %>% mutate(First = "OB1Pro") -> SubIPsOb1ProFirst
SubIPsOb1ProSecond %>% mutate(Second = "OB1Pro") -> SubIPsOb1ProSecond
SubIPsOb1ProThird %>% mutate(Third = "OB1Pro") -> SubIPsOb1ProThird
SubIPsOb1ProFourth %>% mutate(Fourth = "OB1Pro") -> SubIPsOb1ProFourth
SubIPs <- rbind (SubIPsOb1Full,SubIPsOb1ProFirst)
SubIPs <- rbind (SubIPs,SubIPsOb1ProSecond)
SubIPs <- rbind (SubIPs,SubIPsOb1ProThird)
SubIPs <- rbind (SubIPs,SubIPsOb1ProFourth)
SubIPs <- rbind (SubIPs,SubIPsOb1ProRest)

# Encode OB2 as OB2Pro when object is a pronoun
SubIPs %>% filter(Ob2Type == "Pronoun") -> SubIPsOb2Pro
SubIPs %>% filter(Ob2Type != "Pronoun") -> SubIPsOb2Full
SubIPsOb2Pro %>% filter(First == "OB2") -> SubIPsOb2ProFirst
SubIPsOb2Pro %>% filter(Second == "OB2") -> SubIPsOb2ProSecond
SubIPsOb2Pro %>% filter(Third == "OB2") -> SubIPsOb2ProThird
SubIPsOb2Pro %>% filter(Fourth == "OB2") -> SubIPsOb2ProFourth
SubIPsOb2Pro %>% filter(First != "OB2" & Second != "OB2" & Third != "OB2" & Fourth != "OB2") -> SubIPsOb2ProRest
SubIPsOb2ProFirst %>% mutate(First = "OB2Pro") -> SubIPsOb2ProFirst
SubIPsOb2ProSecond %>% mutate(Second = "OB2Pro") -> SubIPsOb2ProSecond
SubIPsOb2ProThird %>% mutate(Third = "OB2Pro") -> SubIPsOb2ProThird
SubIPsOb2ProFourth %>% mutate(Fourth = "OB2Pro") -> SubIPsOb2ProFourth
SubIPs <- rbind (SubIPsOb2Full,SubIPsOb2ProFirst)
SubIPs <- rbind (SubIPs,SubIPsOb2ProSecond)
SubIPs <- rbind (SubIPs,SubIPsOb2ProThird)
SubIPs <- rbind (SubIPs,SubIPsOb2ProFourth)
SubIPs <- rbind (SubIPs,SubIPsOb2ProRest)



#Merge with PLAEME metadata
PLAEME_Info <- read.csv("PLAEME_more_info.csv")
PLAEME_Info$Filename <- tolower(PLAEME_Info$Filename)

ExclusionsList <- c("X","DubiousADVP","OB1Pro","OB2Pro")

MatrixIPs <- merge(MatrixIPs,PLAEME_Info,by="Filename")
MatrixIPs <- subset(MatrixIPs,First != "Conj")
MatrixIPs <- subset(MatrixIPs,First != "EmptySbj")
MatrixIPs <- subset(MatrixIPs,SbjType %in% c("FullNP","Pronoun"))
MatrixIPs <- subset(MatrixIPs,!(First %in% ExclusionsList) & !(Second %in% ExclusionsList) & !(Third %in% ExclusionsList) & !(Fourth %in% ExclusionsList))

SubIPs <- merge(SubIPs,PLAEME_Info,by="Filename")
SubIPs <- subset(SubIPs,First != "Conj")
SubIPs <- subset(SubIPs,First != "EmptySbj")
SubIPs <- subset(SubIPs,SbjType %in% c("FullNP","Pronoun"))
SubIPs <- subset(SubIPs,!(First %in% ExclusionsList) & !(Second %in% ExclusionsList) & !(Third %in% ExclusionsList) & !(Fourth %in% ExclusionsList))


# Add geographical info
MatrixIPs <- subset(MatrixIPs,New.grid.ref != "ZZ000000")
MatrixPoints = osg_parse(MatrixIPs$New.grid.ref,coord_system = "WGS84")
MatrixIPs$lat <- MatrixPoints$lat
MatrixIPs$lon <- MatrixPoints$lon

SubIPs <- subset(SubIPs,New.grid.ref != "ZZ000000")
SubPoints = osg_parse(SubIPs$New.grid.ref,coord_system = "WGS84")
SubIPs$lat <- SubPoints$lat
SubIPs$lon <- SubPoints$lon

# Start making predictions about grammars. Mark everything as no and then add yes on a case by case basis
#Matrix clauses first
XP <- c("ADJP","ADVP","NPAdj","NPArg","OB1","OB2","PP","Ptcp","VP") # Start with list of "what counts as XP"
XPNoObj <- c("ADJP","ADVP","NPAdj","NPArg","PP","Ptcp","VP") # Different list for SVO grammars where Obj should be postverbal?
MatrixIPs$Southern <- "N"
MatrixIPs$Northern <- "N"
MatrixIPs$CM <- "N"
MatrixIPs$SVO <- "N"
#Northern, XP V S YP, Full NP Sbj
MatrixIPs$Northern[MatrixIPs$First %in% XP &
                     MatrixIPs$Second == "V" &
                     MatrixIPs$Third == "S" &
                     MatrixIPs$Fourth %in% XP &
                     MatrixIPs$SbjType == "FullNP"] <- "Y"
#Northern, S V XP YP, Full NP Sbj
MatrixIPs$Northern[MatrixIPs$First == "S" &
                     MatrixIPs$Second == "V" &
                     MatrixIPs$Third %in% XP &
                     MatrixIPs$Fourth %in% XP &
                     MatrixIPs$SbjType == "FullNP"] <- "Y"
#Northern, XP V S YP, Pronoun Sbj
MatrixIPs$Northern[MatrixIPs$First %in% XP &
                     MatrixIPs$Second == "V" &
                     MatrixIPs$Third == "S" &
                     MatrixIPs$Fourth %in% XP &
                     MatrixIPs$SbjType == "Pronoun"] <- "Y"
#Northern, S V XP YP, Pronoun Sbj
MatrixIPs$Northern[MatrixIPs$First == "S" &
                     MatrixIPs$Second == "V" &
                     MatrixIPs$Third %in% XP &
                     MatrixIPs$Fourth %in% XP &
                     MatrixIPs$SbjType == "Pronoun"] <- "Y"
#Southern, XP V YP S, Full NP Sbj
MatrixIPs$Southern[MatrixIPs$First %in% XP &
                     MatrixIPs$Second == "V" &
                     MatrixIPs$Third %in% XP &
                     MatrixIPs$Fourth == "S" &
                     MatrixIPs$SbjType == "FullNP"] <- "Y"
#Southern, XP V S YP, Full NP Sbj
MatrixIPs$Southern[MatrixIPs$First %in% XP &
                     MatrixIPs$Second == "V" &
                     MatrixIPs$Third == "S" &
                     MatrixIPs$Fourth %in% XP &
                     MatrixIPs$SbjType == "FullNP"] <- "Y"
#Southern, S V XP YP, Full NP Sbj
MatrixIPs$Southern[MatrixIPs$First == "S" &
                     MatrixIPs$Second == "V" &
                     MatrixIPs$Third %in% XP &
                     MatrixIPs$Fourth %in% XP &
                     MatrixIPs$SbjType == "FullNP"] <- "Y"
#Southern, XP S V YP, Pronoun Sbj
MatrixIPs$Southern[MatrixIPs$First %in% XP &
                     MatrixIPs$Second == "S" &
                     MatrixIPs$Third == "V" &
                     MatrixIPs$Fourth %in% XP &
                     MatrixIPs$SbjType == "Pronoun"] <- "Y"
#Southern, S V XP YP, Pronoun Sbj
MatrixIPs$Southern[MatrixIPs$First == "S" &
                     MatrixIPs$Second == "V" &
                     MatrixIPs$Third %in% XP &
                     MatrixIPs$Fourth %in% XP &
                     MatrixIPs$SbjType == "Pronoun"] <- "Y"
#CM, XP YP V S, Full NP Sbj
MatrixIPs$CM[MatrixIPs$First %in% XP &
                     MatrixIPs$Second %in% XP &
                     MatrixIPs$Third == "V" &
                     MatrixIPs$Fourth == "S" &
                     MatrixIPs$SbjType == "FullNP"] <- "Y"
#CM, XP V YP S, Full NP Sbj
MatrixIPs$CM[MatrixIPs$First %in% XP &
               MatrixIPs$Second == "V" &
               MatrixIPs$Third %in% XP &
               MatrixIPs$Fourth == "S" &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#CM, XP S V YP, Full NP Sbj
MatrixIPs$CM[MatrixIPs$First %in% XP &
               MatrixIPs$Second == "S" &
               MatrixIPs$Third == "V" &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#CM, XP V S YP, Full NP Sbj
MatrixIPs$CM[MatrixIPs$First %in% XP &
               MatrixIPs$Second == "V" &
               MatrixIPs$Third == "S" &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#CM, S XP V YP, Full NP Sbj
MatrixIPs$CM[MatrixIPs$First == "S" &
               MatrixIPs$Second %in% XP &
               MatrixIPs$Third == "V" &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#CM, S V XP YP, Full NP Sbj
MatrixIPs$CM[MatrixIPs$First == "S" &
               MatrixIPs$Second == "V" &
               MatrixIPs$Third %in% XP &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#CM, XP YP V S, Pronoun Sbj
MatrixIPs$CM[MatrixIPs$First %in% XP &
               MatrixIPs$Second %in% XP &
               MatrixIPs$Third == "V" &
               MatrixIPs$Fourth == "S" &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
#CM, XP S V YP, Pronoun Sbj
MatrixIPs$CM[MatrixIPs$First %in% XP &
               MatrixIPs$Second == "S" &
               MatrixIPs$Third == "V" &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
#CM, XP V S YP, Pronoun Sbj
MatrixIPs$CM[MatrixIPs$First %in% XP &
               MatrixIPs$Second == "V" &
               MatrixIPs$Third == "S" &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
#CM, S XP V YP, Pronoun Sbj
MatrixIPs$CM[MatrixIPs$First == "S" &
               MatrixIPs$Second %in% XP &
               MatrixIPs$Third == "V" &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
#CM, S V XP YP, Pronoun Sbj
MatrixIPs$CM[MatrixIPs$First == "S" &
               MatrixIPs$Second == "V" &
               MatrixIPs$Third %in% XP &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
#SVO, XP YP S V, Full NP Sbj
MatrixIPs$SVO[MatrixIPs$First %in% XPNoObj &
               MatrixIPs$Second %in% XPNoObj &
               MatrixIPs$Third == "S" &
               MatrixIPs$Fourth == "V" &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#SVO, XP S YP V, Full NP Sbj
MatrixIPs$SVO[MatrixIPs$First %in% XPNoObj &
               MatrixIPs$Second == "S" &
               MatrixIPs$Third %in% XP &
               MatrixIPs$Fourth == "V" &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#SVO, XP S V YP, Full NP Sbj
MatrixIPs$SVO[MatrixIPs$First %in% XPNoObj &
               MatrixIPs$Second == "S" &
               MatrixIPs$Third == "V" &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#SVO, S XP YP V, Full NP Sbj
MatrixIPs$SVO[MatrixIPs$First == "S" &
               MatrixIPs$Second %in% XP &
               MatrixIPs$Third %in% XP &
               MatrixIPs$Fourth == "V" &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#SVO, S XP V YP, Full NP Sbj
MatrixIPs$SVO[MatrixIPs$First == "S" &
               MatrixIPs$Second %in% XP &
               MatrixIPs$Third == "V" &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#SVO, S V XP YP, Full NP Sbj
MatrixIPs$SVO[MatrixIPs$First == "S" &
               MatrixIPs$Second == "V" &
               MatrixIPs$Third %in% XP &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "FullNP"] <- "Y"
#SVO, XP YP S V, Pronoun Sbj
MatrixIPs$SVO[MatrixIPs$First %in% XPNoObj &
               MatrixIPs$Second %in% XPNoObj &
               MatrixIPs$Third == "S" &
               MatrixIPs$Fourth == "V" &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
#SVO, XP S YP V, Pronoun Sbj
MatrixIPs$SVO[MatrixIPs$First %in% XPNoObj &
               MatrixIPs$Second == "S" &
               MatrixIPs$Third %in% XP &
               MatrixIPs$Fourth == "V" &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
#SVO, XP S V YP, Pronoun Sbj
MatrixIPs$SVO[MatrixIPs$First %in% XPNoObj &
               MatrixIPs$Second == "S" &
               MatrixIPs$Third == "V" &
               MatrixIPs$Fourth %in% XP &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
#SVO, S XP YP V, Pronoun Sbj
MatrixIPs$SVO[MatrixIPs$First == "S" &
               MatrixIPs$Second %in% XP &
               MatrixIPs$Third %in% XP &
               MatrixIPs$Fourth == "V" &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
#SVO, S XP V YP, Pronoun Sbj
MatrixIPs$SVO[MatrixIPs$First == "S" &
               MatrixIPs$Second %in% XP &
               MatrixIPs$Third == "V" &
               MatrixIPs$Fourth %in% XPNoObj &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
#SVO, S V XP YP, Pronoun Sbj
MatrixIPs$SVO[MatrixIPs$First == "S" &
               MatrixIPs$Second == "V" &
               MatrixIPs$Third %in% XPNoObj &
               MatrixIPs$Fourth %in% XPNoObj &
               MatrixIPs$SbjType == "Pronoun"] <- "Y"
########
# Now subordinate clauses
SubIPs$Southern <- "N"
SubIPs$Northern <- "N"
SubIPs$CM <- "N"
SubIPs$SVO <- "N"
#Northern, S XP YP V, Full NP Sbj
SubIPs$Northern[SubIPs$First == "S" &
                     SubIPs$Second %in% XP &
                     SubIPs$Third %in% XP &
                     SubIPs$Fourth == "V" &
                     SubIPs$SbjType == "FullNP"] <- "Y"
#Northern, S XP V YP, Full NP Sbj
SubIPs$Northern[SubIPs$First == "S" &
                  SubIPs$Second %in% XP &
                  SubIPs$Third == "V" &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "FullNP"] <- "Y"
#Northern, S V XP YP, Full NP Sbj
SubIPs$Northern[SubIPs$First == "S" &
                  SubIPs$Second == "V" &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "FullNP"] <- "Y"
# Northern, XP S V YP, Full NP Sbj
SubIPs$Northern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second == "S" &
                  SubIPs$Third == "V" &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "FullNP"] <- "Y"
# Northern, XP S YP V, Full NP Sbj
SubIPs$Northern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second == "S" &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "FullNP"] <- "Y"
# Northern, XP YP S V, Full NP Sbj
SubIPs$Northern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second %in% XPNoObj &
                  SubIPs$Third == "S" &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "FullNP"] <- "Y"
#Northern, S XP YP V, Pronoun Sbj
SubIPs$Northern[SubIPs$First == "S" &
                  SubIPs$Second %in% XP &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
#Northern, S XP V YP, Pronoun Sbj
SubIPs$Northern[SubIPs$First == "S" &
                  SubIPs$Second %in% XP &
                  SubIPs$Third == "V" &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
#Northern, S V XP YP, Pronoun Sbj
SubIPs$Northern[SubIPs$First == "S" &
                  SubIPs$Second == "V" &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
# Northern, XP S V YP, Pronoun Sbj
SubIPs$Northern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second == "S" &
                  SubIPs$Third == "V" &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
# Northern, XP S YP V, Pronoun Sbj
SubIPs$Northern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second == "S" &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
# Northern, XP YP S V, Pronoun Sbj
SubIPs$Northern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second %in% XPNoObj &
                  SubIPs$Third == "S" &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
#Southern, S XP YP V, Full NP Sbj
SubIPs$Southern[SubIPs$First == "S" &
                  SubIPs$Second %in% XP &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "FullNP"] <- "Y"
#Southern, S XP V YP, Full NP Sbj
SubIPs$Southern[SubIPs$First == "S" &
                  SubIPs$Second %in% XP &
                  SubIPs$Third == "V" &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "FullNP"] <- "Y"
#Southern, S V XP YP, Full NP Sbj
SubIPs$Southern[SubIPs$First == "S" &
                  SubIPs$Second == "V" &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "FullNP"] <- "Y"
# Southern, XP S V YP, Full NP Sbj
SubIPs$Southern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second == "S" &
                  SubIPs$Third == "V" &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "FullNP"] <- "Y"
# Southern, XP S YP V, Full NP Sbj
SubIPs$Southern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second == "S" &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "FullNP"] <- "Y"
# Southern, XP YP S V, Full NP Sbj
SubIPs$Southern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second %in% XPNoObj &
                  SubIPs$Third == "S" &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "FullNP"] <- "Y"
#Southern, XP S V YP, Pronoun Sbj
SubIPs$Southern[SubIPs$First %in% XP &
                  SubIPs$Second == "S" &
                  SubIPs$Third == "V" &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
#Southern, S XP YP V, Pronoun Sbj
SubIPs$Southern[SubIPs$First == "S" &
                  SubIPs$Second %in% XP &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
#Southern, S XP V YP, Pronoun Sbj
SubIPs$Southern[SubIPs$First == "S" &
                  SubIPs$Second %in% XP &
                  SubIPs$Third == "V" &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
#Southern, S V XP YP, Pronoun Sbj
SubIPs$Southern[SubIPs$First == "S" &
                  SubIPs$Second == "V" &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth %in% XP &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
# Southern, XP S YP V, Pronoun Sbj
SubIPs$Southern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second == "S" &
                  SubIPs$Third %in% XP &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
# Southern, XP YP S V, Pronoun Sbj
SubIPs$Southern[SubIPs$First %in% XPNoObj &
                  SubIPs$Second %in% XPNoObj &
                  SubIPs$Third == "S" &
                  SubIPs$Fourth == "V" &
                  SubIPs$SbjType == "Pronoun"] <- "Y"
#CM, XP V S YP, Full NP Sbj
SubIPs$CM[SubIPs$First %in% XP &
            SubIPs$Second == "V" &
            SubIPs$Third == "S" &
            SubIPs$Fourth %in% XP &
            SubIPs$SbjType == "FullNP"] <- "Y"
#CM, S V XP YP, Full NP Sbj
SubIPs$CM[SubIPs$First == "S" &
            SubIPs$Second == "V" &
            SubIPs$Third %in% XP &
            SubIPs$Fourth %in% XP &
            SubIPs$SbjType == "FullNP"] <- "Y"
#CM, XP V S YP, Pronoun Sbj
SubIPs$CM[SubIPs$First %in% XP &
            SubIPs$Second == "V" &
            SubIPs$Third == "S" &
            SubIPs$Fourth %in% XP &
            SubIPs$SbjType == "Pronoun"] <- "Y"
#CM, S V XP YP, Pronoun Sbj
SubIPs$CM[SubIPs$First == "S" &
            SubIPs$Second == "V" &
            SubIPs$Third %in% XP &
            SubIPs$Fourth %in% XP &
            SubIPs$SbjType == "Pronoun"] <- "Y"
#SVO, XP YP S V, Full NP Sbj
SubIPs$SVO[SubIPs$First %in% XPNoObj &
            SubIPs$Second %in% XPNoObj &
            SubIPs$Third == "S" &
            SubIPs$Fourth == "V" &
            SubIPs$SbjType == "FullNP"] <- "Y"
#SVO, XP S YP V, Full NP Sbj
SubIPs$SVO[SubIPs$First %in% XPNoObj &
             SubIPs$Second == "S" &
             SubIPs$Third %in% XP &
             SubIPs$Fourth == "V" &
             SubIPs$SbjType == "FullNP"] <- "Y"
#SVO, XP S V YP, Full NP Sbj
SubIPs$SVO[SubIPs$First %in% XPNoObj &
             SubIPs$Second == "S" &
             SubIPs$Third == "V" &
             SubIPs$Fourth %in% XP &
             SubIPs$SbjType == "FullNP"] <- "Y"
#SVO, S XP YP V, Full NP Sbj
SubIPs$SVO[SubIPs$First == "S" &
             SubIPs$Second %in% XP &
             SubIPs$Third %in% XP &
             SubIPs$Fourth == "V" &
             SubIPs$SbjType == "FullNP"] <- "Y"
#SVO, S XP V YP, Full NP Sbj
SubIPs$SVO[SubIPs$First == "S" &
             SubIPs$Second %in% XP &
             SubIPs$Third == "V" &
             SubIPs$Fourth %in% XP &
             SubIPs$SbjType == "FullNP"] <- "Y"
#SVO, S V XP YP, Full NP Sbj
SubIPs$SVO[SubIPs$First == "S" &
             SubIPs$Second == "V" &
             SubIPs$Third %in% XP &
             SubIPs$Fourth %in% XP &
             SubIPs$SbjType == "FullNP"] <- "Y"
#SVO, XP YP S V, Pronoun Sbj
SubIPs$SVO[SubIPs$First %in% XPNoObj &
             SubIPs$Second %in% XPNoObj &
             SubIPs$Third == "S" &
             SubIPs$Fourth == "V" &
             SubIPs$SbjType == "Pronoun"] <- "Y"
#SVO, XP S YP V, Pronoun Sbj
SubIPs$SVO[SubIPs$First %in% XPNoObj &
             SubIPs$Second == "S" &
             SubIPs$Third %in% XP &
             SubIPs$Fourth == "V" &
             SubIPs$SbjType == "Pronoun"] <- "Y"
#SVO, XP S V YP, Pronoun Sbj
SubIPs$SVO[SubIPs$First %in% XPNoObj &
             SubIPs$Second == "S" &
             SubIPs$Third == "V" &
             SubIPs$Fourth %in% XP &
             SubIPs$SbjType == "Pronoun"] <- "Y"
#SVO, S XP YP V, Pronoun Sbj
SubIPs$SVO[SubIPs$First == "S" &
             SubIPs$Second %in% XP &
             SubIPs$Third %in% XP &
             SubIPs$Fourth == "V" &
             SubIPs$SbjType == "Pronoun"] <- "Y"
#SVO, S XP V YP, Pronoun Sbj
SubIPs$SVO[SubIPs$First == "S" &
             SubIPs$Second %in% XP &
             SubIPs$Third == "V" &
             SubIPs$Fourth %in% XP &
             SubIPs$SbjType == "Pronoun"] <- "Y"
#SVO, S V XP YP, Pronoun Sbj
SubIPs$SVO[SubIPs$First == "S" &
             SubIPs$Second == "V" &
             SubIPs$Third %in% XP &
             SubIPs$Fourth %in% XP &
             SubIPs$SbjType == "Pronoun"] <- "Y"

#Make single big df
AllIPs <- rbind(MatrixIPs,SubIPs)

#Matrix and subordinate general summary data 
MatrixIPs %>% group_by(lat,lon,Words,Filename) %>% summarize(Total = n()) -> Matrix_counts
SubIPs %>% group_by(lat,lon,Words,Filename) %>% summarize(Total = n()) -> Sub_counts
AllIPs %>% group_by(lat,lon,Words,Filename) %>% summarize(Total = n()) -> All_counts
Unique_counts <- All_counts

#Summary data for different grammars
#Matrix
MatrixNorthern <- subset(MatrixIPs,Northern == "Y")
MatrixNorthern %>% group_by(lat,lon,Words,Filename) %>% summarize(Northern = n()) -> Northern_counts
MatrixSouthern <- subset(MatrixIPs,Southern == "Y")
MatrixSouthern %>% group_by(lat,lon,Words,Filename) %>% summarize(Southern = n()) -> Southern_counts
MatrixCM <- subset(MatrixIPs,CM == "Y")
MatrixCM %>% group_by(lat,lon,Words,Filename) %>% summarize(CM = n()) -> CM_counts
MatrixSVO <- subset(MatrixIPs,SVO == "Y")
MatrixSVO %>% group_by(lat,lon,Words,Filename) %>% summarize(SVO = n()) -> SVO_counts
Matrix_counts <- merge(Matrix_counts,Northern_counts,all=T)
Matrix_counts <- merge(Matrix_counts,Southern_counts,all=T)
Matrix_counts <- merge(Matrix_counts,CM_counts,all=T)
Matrix_counts <- merge(Matrix_counts,SVO_counts,all=T)
Matrix_counts[is.na(Matrix_counts)] <- 0
#Sub
SubNorthern <- subset(SubIPs,Northern == "Y")
SubNorthern %>% group_by(lat,lon,Words,Filename) %>% summarize(Northern = n()) -> Northern_counts
SubSouthern <- subset(SubIPs,Southern == "Y")
SubSouthern %>% group_by(lat,lon,Words,Filename) %>% summarize(Southern = n()) -> Southern_counts
SubCM <- subset(SubIPs,CM == "Y")
SubCM %>% group_by(lat,lon,Words,Filename) %>% summarize(CM = n()) -> CM_counts
SubSVO <- subset(SubIPs,SVO == "Y")
SubSVO %>% group_by(lat,lon,Words,Filename) %>% summarize(SVO = n()) -> SVO_counts
Sub_counts <- merge(Sub_counts,Northern_counts,all=T)
Sub_counts <- merge(Sub_counts,Southern_counts,all=T)
Sub_counts <- merge(Sub_counts,CM_counts,all=T)
Sub_counts <- merge(Sub_counts,SVO_counts,all=T)
Sub_counts[is.na(Sub_counts)] <- 0
#All
AllNorthern <- subset(AllIPs,Northern == "Y")
AllNorthern %>% group_by(lat,lon,Words,Filename) %>% summarize(Northern = n()) -> Northern_counts
AllSouthern <- subset(AllIPs,Southern == "Y")
AllSouthern %>% group_by(lat,lon,Words,Filename) %>% summarize(Southern = n()) -> Southern_counts
AllCM <- subset(AllIPs,CM == "Y")
AllCM %>% group_by(lat,lon,Words,Filename) %>% summarize(CM = n()) -> CM_counts
AllSVO <- subset(AllIPs,SVO == "Y")
AllSVO %>% group_by(lat,lon,Words,Filename) %>% summarize(SVO = n()) -> SVO_counts
All_counts <- merge(All_counts,Northern_counts,all=T)
All_counts <- merge(All_counts,Southern_counts,all=T)
All_counts <- merge(All_counts,CM_counts,all=T)
All_counts <- merge(All_counts,SVO_counts,all=T)
All_counts[is.na(All_counts)] <- 0
# #Unique (CM and SVO only because 0 for northern and 7 for southern)
# UniquelyCM <- subset(AllIPs,Northern == "N" & Southern == "N" & CM == "Y" & SVO == "N")
# UniquelyCM %>% group_by(lat,lon,Words,Filename) %>% summarize(CM = n()) -> Unique_CM_counts
# UniquelySVO <- subset(AllIPs,Northern == "N" & Southern == "N" & CM == "N" & SVO == "Y")
# UniquelySVO %>% group_by(lat,lon,Words,Filename) %>% summarize(SVO = n()) -> Unique_SVO_counts
# Unique_counts <- merge(Unique_counts,Unique_CM_counts,all=T)
# Unique_counts <- merge(Unique_counts,Unique_SVO_counts,all=T)
# Unique_counts[is.na(Unique_counts)] <- 0

# Basic England map

# latmin=49.9
# latmax=55.8
# lonmin=-6
# lonmax=2
# # England <- get_stamenmap(bbox=c(left=lonmin,bottom=latmin,right=lonmax,top=latmax),maptype="terrain-background",zoom=7,force=F,crop=T)

# Basic England map
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

#Colour palette for points
# MyColours <- c("#000000","#330000","#CC0000","#7FCC00","#B2FF66")
MyColours <- c("#000000","#CC0066","#7FCC55","#B2FF44") # Added this
MyBouncyColours <- c("#330000","#80FF00","#B2FF66","#99FF99")

#Northern matrix map
MatrixNorthern.gg <- England.gg +
  geom_point(data=Matrix_counts,aes(y=lat,x=lon,size=Total,color=Northern/Total)) +
  scale_color_gradientn(colours=MyColours,name="% Northern") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(0,0,5,0),"mm"))
#Southern matrix map
MatrixSouthern.gg <- England.gg +
  geom_point(data=Matrix_counts,aes(y=lat,x=lon,size=Total,color=Southern/Total)) +
  scale_color_gradientn(colours=MyColours,name="% Southern") + 
  #scale_color_continuous(name="% Southern") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(0,0,5,0),"mm"))
#CM matrix map
MatrixCM.gg <- England.gg +
  geom_point(data=Matrix_counts,aes(y=lat,x=lon,size=Total,color=CM/Total)) +
  #scale_color_continuous(name="% CM") + 
  scale_color_gradientn(colours=MyColours,name="% CM") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(5,0,0,0),"mm"))
#SVO matrix map
MatrixSVO.gg <- England.gg +
  geom_point(data=Matrix_counts,aes(y=lat,x=lon,size=Total,color=SVO/Total)) +
  #scale_color_continuous(name="% SV") + 
  scale_color_gradientn(colours=MyColours,name="% SV") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(5,0,0,0),"mm"))

#Northern sub map
SubNorthern.gg <- England.gg +
  geom_point(data=Sub_counts,aes(y=lat,x=lon,size=Total,color=Northern/Total)) +
  scale_color_gradientn(colours=MyColours,name="% Northern") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(0,0,5,0),"mm"))
#Southern sub map
SubSouthern.gg <- England.gg +
  geom_point(data=Sub_counts,aes(y=lat,x=lon,size=Total,color=Southern/Total)) +
  scale_color_gradientn(colours=MyColours,name="% Southern") + 
  #scale_color_continuous(name="% Southern") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(0,0,5,0),"mm"))
#CM sub map
SubCM.gg <- England.gg +
  geom_point(data=Sub_counts,aes(y=lat,x=lon,size=Total,color=CM/Total)) +
  #scale_color_continuous(name="% CM") + 
  scale_color_gradientn(colours=MyColours,name="% CM") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(5,0,0,0),"mm"))
#SVO sub map
SubSVO.gg <- England.gg +
  geom_point(data=Sub_counts,aes(y=lat,x=lon,size=Total,color=SVO/Total)) +
  #scale_color_continuous(name="% SV") + 
  scale_color_gradientn(colours=MyColours,name="% SV") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(5,0,0,0),"mm"))

#Northern all map
AllNorthern.gg <- England.gg +
  geom_point(data=All_counts,aes(y=lat,x=lon,size=Total,color=Northern/Total)) +
  scale_color_gradientn(colours=MyColours,name="% Northern") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(0,0,5,0),"mm"))
#Southern all map
AllSouthern.gg <- England.gg +
  geom_point(data=All_counts,aes(y=lat,x=lon,size=Total,color=Southern/Total)) +
  scale_color_gradientn(colours=MyColours,name="% Southern") + 
  #scale_color_continuous(name="% Southern") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(0,0,5,0),"mm"))
#CM all map
AllCM.gg <- England.gg +
  geom_point(data=All_counts,aes(y=lat,x=lon,size=Total,color=CM/Total)) +
  #scale_color_continuous(name="% CM") + 
  scale_color_gradientn(colours=MyColours,name="% CM") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(5,0,0,0),"mm"))
#SVO all map
AllSVO.gg <- England.gg +
  geom_point(data=All_counts,aes(y=lat,x=lon,size=Total,color=SVO/Total)) +
  #scale_color_continuous(name="% SV") + 
  scale_color_gradientn(colours=MyColours,name="% SV     ") + 
  scale_size_area(name = "Tokens") +
  theme_void() +
  theme(plot.margin = unit(c(5,0,0,0),"mm"))

# #CM unique map
# UniqueCM.gg <- England.gg +
#   geom_point(data=Unique_counts,aes(y=lat,x=lon,size=Total,color=CM/Total)) +
#   #scale_color_continuous(name="% CM") + 
#   scale_color_gradientn(colours=MyBouncyColours,name="% CM") + 
#   scale_size_area(name = "Tokens") +
#   theme_void()
# #SVO unique map
# UniqueSVO.gg <- England.gg +
#   geom_point(data=Unique_counts,aes(y=lat,x=lon,size=Total,color=SVO/Total)) +
#   #scale_color_continuous(name="% SV") + 
#   scale_color_gradientn(colours=MyBouncyColours,name="% SV") + 
#   scale_size_area(name = "Tokens") +
#   theme_void()

#Make pdfs
pdf("MatrixMaps_new.pdf",height = 10, width = 12)
grid.arrange(MatrixNorthern.gg,MatrixSouthern.gg,MatrixCM.gg,MatrixSVO.gg,ncol=2,padding=unit(1,"line"))
dev.off()

pdf("SubMaps_new.pdf",height = 10, width = 12)
grid.arrange(SubNorthern.gg,SubSouthern.gg,SubCM.gg,SubSVO.gg,ncol=2)
dev.off()

pdf("AllMaps_new.pdf",height = 10, width = 12)
grid.arrange(AllNorthern.gg,AllSouthern.gg,AllCM.gg,AllSVO.gg,ncol=2)
dev.off()
