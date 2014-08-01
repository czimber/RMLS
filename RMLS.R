# Get File
z <- read.csv("/Users/craigzimber/Documents/R/RMLS/Data/W - Condo 148 140729 - Working.csv", 
                header=TRUE, sep = ",", 
                # na.strings = "", 
                stringsAsFactors = TRUE)            

# z$Download.Date <- "2014-07-28"  # If need Download.Date
# z$Download.Date <- as.Date(z$Download.Date)

# Removes rows that dont start with a MLS#
mls <- subset(z,z$Status == "ACT" | z$Status == "BMP" | z$Status == "CAN" | 
                z$Status == "EXP" | z$Status == "PEN" | z$Status == "SLD" | 
                z$Status == "SNL" | z$Status == "SSP" | z$Status == "WTH")
dim(mls)

# Rows that dont start with a MLS#
miss <- subset(z,z$Status != "ACT" & z$Status != "BMP" & z$Status != "CAN" & 
                 z$Status != "EXP" & z$Status != "PEN" & z$Status != "SLD" & 
                 z$Status != "SNL" & z$Status != "SSP" & z$Status != "WTH")
dim(miss)

# Split out Price Range
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

mls.lr <- mls

# Give Classes
mls.lr$HOA.Amt <- as.numeric(mls.lr$HOA.Amt)
mls.lr$O.Price <- as.numeric(mls.lr$O.Price)
mls.lr$Close.Price <- as.numeric(mls.lr$Close.Price)
mls.lr$CDOM <- as.numeric(mls.lr$CDOM)

#
mls.lr$Inter.HARDWOD <- NA  # Setting up HARDWOD 
mls.lr$Inter.HARDWOD <- str_detect(mls.lr$Inter, "HARDWOD")
#
mls.lr$Inter.LAM.FL <- NA  # Setting up LAM-FL 
mls.lr$Inter.LAM.FL <- str_detect(mls.lr$Inter, "LAM-FL")
#
mls.lr$Inter.WOODFLR <- NA  # Setting up WOODFLR 
mls.lr$Inter.WOODFLR <- str_detect(mls.lr$Inter, "WOODFLR")
#
mls.lr$Inter.TILE.FL <- NA  # Setting up TILE-FL
mls.lr$Inter.TILE.FL <- str_detect(mls.lr$Inter, "TILE-FL")
#
mls.lr$Inter.VNYL.FL <- NA  # Setting up VNYL-FL
mls.lr$Inter.VNYL.FL <- str_detect(mls.lr$Inter, "VNYL-FL")
#
mls.lr$Inter.BAMB.FL <- NA  # Setting up BAMB-FL
mls.lr$Inter.BAMB.FL <- str_detect(mls.lr$Inter, "BAMB-FL")
#
mls.lr$Inter.WW.CARP <- NA  # Setting up WW-CARP
mls.lr$Inter.WW.CARP <- str_detect(mls.lr$Inter, "WW-CARP")
#
mls.lr$Inter.SLATEFL <- NA  # Setting up SLATEFLR
mls.lr$Inter.SLATEFL <- str_detect(mls.lr$Inter, "SLATEFL")

mls.lr$FIXER <- NA  # Setting up FIXER
mls.lr$FIXER <- str_detect(mls.lr$Year.Built.Description, "FIXER")

mls.lr$CASH.ONLY <- NA  # Setting up CASH.ONLY
mls.lr$CASH.ONLY[mls.lr$Terms == "CASH"] <- "TRUE"

mls.lr$CASH.ONLY[mls.lr$Terms == "CASH"] <- TRUE
mls.lr$CASH.ONLY[mls.lr$Terms != "CASH"] <- FALSE

mls.lr$CASH.ONLY <- str_detect(mls.lr$Year.Built.Description, "FIXER")

# Change to upper: Address, TaxID, Legal
mls.lr$Address <- toupper(mls.lr$Address)
mls.lr$Tax.ID <- toupper(mls.lr$Tax.ID)
mls.lr$Legal <- toupper(mls.lr$Legal)

# as.factor
mls.lr$Zip.Code <- as.factor(mls.lr$Zip.Code)
mls.lr$Plus.4 <- as.factor(mls.lr$Plus.4)
mls.lr$Area <- as.factor(mls.lr$Area)
mls.lr$Tax.ID <- as.factor(mls.lr$Tax.ID)

# as.Date
mls.lr$List.Date <- as.Date(mls.lr$List.Date, format="%m/%d/%Y")
mls.lr$Close.Date <- as.Date(mls.lr$Close.Date, format="%m/%d/%Y")
mls.lr$Expiration.Date <- as.Date(mls.lr$Expiration.Date, format="%m/%d/%Y")
mls.lr$Pend <- as.Date(mls.lr$Pend, format="%m/%d/%Y")



median(mls.lr$CDOM, na.rm=TRUE)
table(mls.lr$Short.Sale)

# names(mls.lr)
sum(mls.lr$Inter.HARDWOD, na.rm = TRUE) 
sum(mls.lr$Inter.LAM.FL, na.rm = TRUE)
sum(mls.lr$Inter.WOODFLR, na.rm = TRUE)
sum(mls.lr$Inter.TILE.FL, na.rm = TRUE)
sum(mls.lr$Inter.BAMB.FL, na.rm = TRUE)
sum(mls.lr$Inter.VNYL.FL, na.rm = TRUE)
sum(mls.lr$Inter.WW.CARP, na.rm = TRUE)
sum(mls.lr$Inter.SLATEFL, na.rm = TRUE)

dim(mls.lr)

summary(mls.lr$CDOM)
summary(mls.lr$List.Price)
summary(mls.lr$Max.Price)
summary(mls.lr$Inter.BAMB.FL)



# ?stop

# Subset needed columns
# z <- mls.lr[,c(1,6,7,4,5,8:11,17,2,3,15,18,19,21:24,37:41,43:50,61,
#               94,96,104,118)]

# zz <- mls.lr[,380:385]
dim(mls.lr)

z[1:5,]

write.csv(mls.lr, "RMLS_Data_Condo_148_140729.csv", row.names=FALSE)


###   write.table(data, "data.csv", sep="\t", row.names=FALSE, col.names=FALSE) 
# z <- as.data.frame(table(mls.lr$High))
# z           
# zz <- sort(z,z$Freq)
# zz <- z[with(z, order(-Freq)), ]
# zz           
# mls.lr$Zip.Code <- as.factor(mls.lr$Zip.Code)
#           str(mls.lr)
           
#           write.csv(mls.lr, "RMLS_Data_140630.csv", row.names=FALSE)
           
           
#           r1 <- read.csv("/Users/craigzimber/Documents/R/RMLS/RMLS_Data_140630.csv", 
#                    header=TRUE, sep = ",", na.strings = "")
           
#           str(r1)
           
           
           
           