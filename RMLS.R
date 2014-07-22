# Full week download
###   r1 <- read.csv("/Users/craigzimber/Documents/R/RMLS/RMLS 141-152 Last 30 140630.csv", 
#           header=TRUE, sep = ",", na.strings = "")

###   r2 <- read.csv("/Users/craigzimber/Documents/R/RMLS/RMLS 141-152 Last 30 140707.csv", 
#               header=TRUE, sep = ",", na.strings = "")

# Half week download
#r1 <- read.csv("/Users/craigzimber/Documents/R/RMLS/RMLS All 148-152 Last 30 140630.csv", 
#               header=TRUE, sep = ",", na.strings = "")
#r2 <- read.csv("/Users/craigzimber/Documents/R/RMLS/RMLS All 141-147 Last 30 140630.csv", 
#               header=TRUE, sep = ",", na.strings = "")
          
#mls <- rbind(r1,r2)

           
# Week download
z <- read.csv("/Users/craigzimber/Documents/R/RMLS/RMLS 141-152 Last 30 140720.csv", 
                          header=TRUE, sep = ",", 
              #na.strings = "", 
              stringsAsFactors = TRUE)            

z$Download.Date <- "2014-07-20"
z$Download.Date <- as.Date(z$Download.Date)
str(z)
#removes rows that dont start with a MLS#
mls <- subset(z,z$Status == "ACT" | z$Status == "BMP" | z$Status == "CAN" | 
                      z$Status == "EXP" | z$Status == "PEN" | z$Status == "SLD" | 
                      z$Status == "SNL" | z$Status == "SSP" | z$Status == "WTH")
dim(mls)

# rows that dont start with a MLS#
miss <- subset(z,z$Status != "ACT" & z$Status != "BMP" & z$Status != "CAN" & 
                       z$Status != "EXP" & z$Status != "PEN" & z$Status != "SLD" & 
                       z$Status != "SNL" & z$Status != "SSP" & z$Status != "WTH")
dim(miss)
### Split out Price Range
z <- mls[,c(1,4)]
z1 <- do.call(rbind.data.frame, strsplit(as.character(z$List.Price), "-"))
        colnames(z1)[1] <- "List"
        colnames(z1)[2] <- "Max"
z <- cbind(z,z1)
        ListPrice <- as.numeric(levels(z$List[])[z$List[]])
        MaxPrice <- as.numeric(levels(z$Max[])[z$Max[]])
z <- cbind(z,ListPrice,MaxPrice)

j<-nrow(z) 
        for (i in 1:j ) {   
        if(z[i,5] == z[i,6])
        {z[i,7] <- NA
        } else {
                z[i,7] <- z[i,6]
        }
}

z <- z[,-c(2,3,4,6)]                

colnames(z)[2] <- "List.Price"
colnames(z)[3] <- "Max.Price"

mls <- cbind(z,mls)
mls <-mls[,-c(4,7)]

# Cleans Units
require(stringr)
j <- nrow(mls)
        for (i in 1:j ) {   
        if(str_detect(mls[i,7], "[lL][oO][tT]")) { # removes "Lot"
                mls[i,7] <- ""
        }}

mls[,7] <- gsub("-", "",mls[,7])
mls[,7] <- gsub("#", "",mls[,7])
mls[,7] <- gsub(" ", "",mls[,7])
mls[,7] <- gsub(" ", "",mls[,7])
mls$Unit.. <- as.factor(mls$Unit..)

# Split out Zip Plus 4
z <- mls[,c(1,10)]
z1 <- do.call(rbind.data.frame, strsplit(as.character(z$Zip.Code), "-"))

colnames(z1)[1] <- "Zip"
colnames(z1)[2] <- "P4"

z <- cbind(z,z1)
ZipCode <- as.numeric(levels(z$Zip[])[z$Zip[]])
Plus4 <- as.numeric(levels(z$P4[])[z$P4[]])
z <- cbind(z,ZipCode,Plus4)
z$Plus.4 <- NA

#
j<-nrow(z) 
for (i in 1:j ) {   
        if(z[i,5] == z[i,6])
        {z[i,7] <- NA
        } else {
                z[i,7] <- z[i,6]
        }
}
z <- z[,-c(2,3,4,6)]                
colnames(z)[2] <- "Zip.Code"
mls <- cbind(z,mls)
mls <-mls[,-c(4,13)]

summary(mls$LR)
##################### GOOD TO HERE 
#### Change Y N to 1 0 
mls.lr <- mls

#mls.lr <- data.frame(lapply(mls.lr, as.character), stringsAsFactors=FALSE)
#mls.lr$LR <- as.character(mls.lr$LR)
#mls.lr$LR[mls.lr$LR == "N"] <- "FALSE"
#mls.lr$LR[mls.lr$LR == "Y"] <- "TRUE"
#mls.lr$LR <- as.factor(mls.lr$LR)


#mls.lr$Home.Warranty
#mls.lr$Home.Warranty <- as.character(mls.lr$Home.Warranty)
#mls.lr$Home.Warranty[mls.lr$Home.Warranty == "N"] <- "FALSE" 
#mls.lr$Home.Warranty[mls.lr$Home.Warranty == "Y"] <- "TRUE"
#mls.lr$Home.Warranty <- as.factor(mls.lr$Home.Warranty)



#mls.lr$Tax.Deferral[mls.lr$Tax.Deferral == "N"] <- 0
#mls.lr$Tax.Deferral[mls.lr$Tax.Deferral == "Y"] <- 1
#mls.lr$X3rd.Party[mls.lr$X3rd.Party == "N"] <- 0
#mls.lr$X3rd.Party[mls.lr$X3rd.Party == "Y"] <- 1
#mls.lr$Bank.Owned.REO[mls.lr$Bank.Owned.REO == "N"] <- 0
#mls.lr$Bank.Owned.REO[mls.lr$Bank.Owned.REO == "Y"] <- 1
#mls.lr$Auction.Y.N[mls.lr$Auction.Y.N == "N"] <- 0
#mls.lr$Auction.Y.N[mls.lr$Auction.Y.N == "Y"] <- 1
#mls.lr$Short.Sale[mls.lr$Short.Sale == "N"] <- 0
#mls.lr$Short.Sale[mls.lr$Short.Sale == "Y"] <- 1

#mls.lr$LR <- as.numeric(mls.lr$LR)
#mls.lr$Home.Warranty <- as.numeric(mls.lr$Home.Warranty)
#mls.lr$Tax.Deferral <- as.numeric(mls.lr$Tax.Deferral)

mls.lr$HOA.Amt <- as.numeric(mls.lr$HOA.Amt)

#mls.lr$X3rd.Party <- as.numeric(mls.lr$X3rd.Party)
#mls.lr$Bank.Owned.REO <- as.numeric(mls.lr$Bank.Owned.REO)
mls.lr$O.Price <- as.numeric(mls.lr$O.Price)
mls.lr$Close.Price <- as.numeric(mls.lr$Close.Price)
# mls.lr$Close.Date <- as.Date(mls.lr$Close.Date)
mls.lr$CDOM <- as.numeric(mls.lr$CDOM)
#mls.lr$Auction.Y.N <- as.numeric(mls.lr$Auction.Y.N)
#mls.lr$Short.Sale <- as.numeric(mls.lr$Short.Sale)

#

mls.lr$Inter.HARDWOD <- NA # Setting up HARDWOD 
mls.lr$Inter.HARDWOD <- str_detect(mls.lr$Inter, "HARDWOD")
#mls.lr$Inter.HARDWOD[mls.lr$Inter.HARDWOD == "FALSE"] <- 0
#mls.lr$Inter.HARDWOD[mls.lr$Inter.HARDWOD == "TRUE"] <- 1
#
mls.lr$Inter.LAM.FL <- NA # Setting up LAM-FL 
mls.lr$Inter.LAM.FL <- str_detect(mls.lr$Inter, "LAM-FL")
#mls.lr$Inter.LAM.FL[mls.lr$Inter.LAM.FL == "FALSE"] <- 0
#mls.lr$Inter.LAM.FL[mls.lr$Inter.LAM.FL == "TRUE"] <- 1
#
mls.lr$Inter.WOODFLR <- NA # Setting up WOODFLR 
mls.lr$Inter.WOODFLR <- str_detect(mls.lr$Inter, "WOODFLR")
#mls.lr$Inter.WOODFLR[mls.lr$Inter.WOODFLR == "FALSE"] <- 0
#mls.lr$Inter.WOODFLR[mls.lr$Inter.WOODFLR == "TRUE"] <- 1
#
mls.lr$Inter.TILE.FL <- NA # Setting up TILE-FL
mls.lr$Inter.TILE.FL <- str_detect(mls.lr$Inter, "TILE-FL")
#mls.lr$Inter.TILE.FL[mls.lr$Inter.TILE.FL == "FALSE"] <- 0
#mls.lr$Inter.TILE.FL[mls.lr$Inter.TILE.FL == "TRUE"] <- 1
#
mls.lr$Inter.VNYL.FL <- NA # Setting up VNYL-FL
mls.lr$Inter.VNYL.FL <- str_detect(mls.lr$Inter, "VNYL-FL")
#mls.lr$Inter.VNYL.FL[mls.lr$Inter.VNYL.FL == "FALSE"] <- 0
#mls.lr$Inter.VNYL.FL[mls.lr$Inter.VNYL.FL == "TRUE"] <- 1
#
mls.lr$Inter.BAMB.FL <- NA # Setting up BAMB-FL
mls.lr$Inter.BAMB.FL <- str_detect(mls.lr$Inter, "BAMB-FL")
#mls.lr$Inter.BAMB.FL[mls.lr$Inter.BAMB.FL == "FALSE"] <- 0
#mls.lr$Inter.BAMB.FL[mls.lr$Inter.BAMB.FL == "TRUE"] <- 1
#
mls.lr$Inter.WW.CARP <- NA # Setting up WW-CARP
mls.lr$Inter.WW.CARP <- str_detect(mls.lr$Inter, "WW-CARP")
#mls.lr$Inter.WW.CARP[mls.lr$Inter.WW.CARP == "FALSE"] <- 0
#mls.lr$Inter.WW.CARP[mls.lr$Inter.WW.CARP == "TRUE"] <- 1
#
mls.lr$Inter.SLATEFL <- NA # Setting up SLATEFLR
mls.lr$Inter.SLATEFL <- str_detect(mls.lr$Inter, "SLATEFL")
#mls.lr$Inter.SLATEFL[mls.lr$Inter.SLATEFL == "FALSE"] <- 0
#mls.lr$Inter.SLATEFL[mls.lr$Inter.SLATEFL == "TRUE"] <- 1
#mls.lr$Inter.SLATEFL
#mls.lr$Inter[1:100]

####
median(mls.lr$CDOM, na.rm=TRUE)
table(mls.lr$Short.Sale)
sum(mls.lr$Short.Sale, na.rm = "Y")
max(mls.lr$Short.Sale, na.rm = TRUE)
####
#   names(mls.lr)
sum(mls.lr$Inter.HARDWOD, na.rm = TRUE) 
sum(mls.lr$Inter.LAM.FL, na.rm = TRUE)
sum(mls.lr$Inter.WOODFLR, na.rm = TRUE)
sum(mls.lr$Inter.TILE.FL, na.rm = TRUE)
sum(mls.lr$Inter.BAMB.FL, na.rm = TRUE)
sum(mls.lr$Inter.VNYL.FL, na.rm = TRUE)
sum(mls.lr$Inter.WW.CARP, na.rm = TRUE)
sum(mls.lr$Inter.SLATEFL, na.rm = TRUE)

mls.lr$Download.Date <- "2014/07/20"
mls.lr$Download.Date <- as.Date(mls.lr$Download.Date)
###   mls.lr[1:20,380:382]
###   class(mls.lr$Download.Date)
dim(mls.lr)

summary(mls.lr$CDOM)
mls.lr[1:5,380:382]
#write.csv(mls.lr, "RMLS_Data_140630.csv", row.names=FALSE)
###   write.table(data, "data.csv", sep="\t", row.names=FALSE, col.names=FALSE) 
z <- as.data.frame(table(mls.lr$High))
z           
zz <- sort(z,z$Freq)
zz <- z[with(z, order(-Freq)), ]
zz           
mls.lr$Zip.Code <- as.factor(mls.lr$Zip.Code)
           str(mls.lr)
           
           write.csv(mls.lr, "RMLS_Data_140630.csv", row.names=FALSE)
           
           
           r1 <- read.csv("/Users/craigzimber/Documents/R/RMLS/RMLS_Data_140630.csv", 
                    header=TRUE, sep = ",", na.strings = "")
           
           str(r1)
           
           
           
           