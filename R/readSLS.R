#
# # # Function that reads data
# # readSLS <- function(file){
# #   if()
# # }
#
# read.sim <- function(file){
#   if(!checkExt(file)){
#     return("Error: The extension of the file do not match the correct form")
#   }else{
#     dat=scan(file = file,what = "character",sep = "\n")
#     LW <- as.numeric(str_split(string = str_split(string = dat[3],pattern = "#")[[1]][1],pattern = ",")[[1]][1:2])
#     GF <- as.numeric(str_split(string = str_split(string = dat[4],pattern = "#")[[1]][1],pattern = ",")[[1]][1:2])
#     cstNM <- as.numeric(str_split(string = dat[5],pattern = " ")[[1]][1])
#     valNM <- as.numeric(str_split(string = dat[6],pattern = " ")[[1]][1])
#     proErr <- as.numeric(str_split(string = dat[7],pattern = " ")[[1]][1])
#     obsErr <- as.numeric(str_split(string = dat[8],pattern = " ")[[1]][1])
#     nbrSim <- as.numeric(str_split(string = dat[9],pattern = " ")[[1]][1])
#     season <- as.numeric(str_split(string = dat[10],pattern = " ")[[1]][1])
#     nbrFleet <- as.numeric(str_split(string = dat[11],pattern = " ")[[1]][1])
#     nbrZone <- as.numeric(str_split(string = dat[12],pattern = " ")[[1]][1])
#     discard <- as.numeric(str_split(string = dat[13],pattern = " ")[[1]][1])
#     recTot <- as.numeric(str_split(string = dat[14],pattern = " ")[[1]][1])
#     initLenghts <- as.numeric(str_split(string = str_split(string = dat[15],pattern = "#")[[1]][1],pattern = ",")[[1]][1:nbrZone])
#     propRec<- as.numeric(str_split(string = str_split(string = dat[16],pattern = "#")[[1]][1],pattern = ",")[[1]][1:nbrZone])
#     initAbund <- as.numeric(str_split(string = str_split(string = dat[17],pattern = "#")[[1]][1],pattern = ",")[[1]][1:nbrZone])
#     catchability <- as.numeric(str_split(string = str_split(string = dat[18],pattern = "#")[[1]][1],pattern = ",")[[1]][1:nbrFleet])
#     codeData <- str_trim(as.character(str_split(string = dat[19],pattern = "#")[[1]][1]))
#     fleetName <- as.character(str_trim(str_split(string = str_split(string = dat[20],pattern = "#")[[1]][1],pattern = ",")[[1]][1:nbrFleet]))
#     zoneName <- rep("",nbrZone)
#     dt.array <- array(data = NA,dim = c(season,nbrFleet+2,nbrZone))
#     index=1
#     j=0
#     while(index<nbrZone+1){
#       j=21+(index-1)*(season+1)
#       zoneName[index]=str_trim(as.character(str_split(string = dat[j],pattern = " ")[[1]][1]))
#       for(i in 1:season){
#         dt.array[i,,index] <-  as.numeric(unlist(str_split(string = str_split(string = dat[j+i],pattern = c(" "))[[1]],pattern = "\t")))
#       }
#       index=index+1
#     }
#   }
#   dataList=list("LW"=LW,"GF"=GF,"cstNM"=cstNM,"valNM"=valNM,"proErr"=proErr,"obsErr"=obsErr,"nbrSim"=nbrSim,"season"=season,"nbrFleet"=nbrFleet,"nbrZone"=nbrZone,"discard"=discard,"recTot"=recTot,"initLenghts"=initLenghts,"propRec"=propRec,"initAbund"=initAbund,"catchability"=catchability,"nbrFPC"=nbrFPC,"FPC"=FPC,"codeData"=codeData,"fleetName"=fleetName,"zoneName"=zoneName,"dt.array"=dt.array)
#   return(dataList)
# }
