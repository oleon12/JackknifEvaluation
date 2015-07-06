####Jackknife resample function
##

#First we need
# Number of replicactions

rep <- c(10)

#Second
#Resample percentage per replication

resam <- c(25)

#############################################################
#Read the file

file <- file("licopodiophyta.dat.xyd",open="r")
xyd <- readLines(file) ## Read file's lines.

##
#Split characters in single words

xyd2 <- c()

for (i in 1:length(xyd)){
  xyd2[i] <- strsplit(xyd[i],"[ ]") ##Split i vector in the list by space "[ ]"
}

############################################################
# Delete enters from the xyd 

na <- c()

for(i in 1:length((xyd2))){
  na[i] <- "sp"==xyd2[[i]][1]
}

no.na <- which(is.na(na)==TRUE)
xyd2 <- xyd2[-no.na]


############################################################
# Define the begining of the data

a <- which("xydata"==xyd2)

begin.data <- a+1 ## In this position the occurences data begins

############################################################
# Occurences separation from the data.

occ <- c(rep(NA, length(xyd2))) ##Empty vector for the occurences position in the xyd2 

for(i in begin.data:length(xyd2)){
  aa<- "sp"==xyd2[[i]][1] 
  if((aa==FALSE)==TRUE){occ[i] <- i}
  ## Only the position of the occurrence in the xyd will be assigned on the vector- 
}

occ <-  occ[-which(is.na(occ)==TRUE)]  ##Remove NA values
occ 
###########################################################
# Resample

resam.occ <- (resam*length(xyd2))/100 ## Get the total number of occurences what will be delete given the percentage resample

p.occ <- sample(occ,size = resam.occ)

nn1 <- c(rep("jack",rep))
nn2 <- seq(from=1,to=rep)
nn3 <- c(rep("xyd",rep))
nn4 <- paste(nn2,nn1,sep = ".") ##file's names
names <- paste(nn4,nn3,sep = ".")

for (i in 1:rep){
  p.occ <- sample(occ,size = resam.occ)
  xyd3.jack <- xyd2[-p.occ]
  a.c <- as.character(xyd3.jack)
  cat(a.c,file=names[i],sep="\n")
}

try(system("sh toXYD.sh"))
# JackknifEvaluation
# JackknifEvaluation
