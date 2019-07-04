############################################################
#' A Parts App Function
#'
#' This function allows you to Identify, extract and merge multiple partsapps
#' @param Defaults to TRUE.
#' @keywords Parts App
#' @export
#' @examples
#' pooledPartsApp()
###########################################################
REF.Conversion,
PartsApp <- function(DCIFolderList, Update.PostIF.Merge ){


message("--------------------------**")


#Create empty Data frame to collect the multiple DCI folder data
PooledPartsApp = data.frame()


#Find the latest file within the specific DCI folder
##Change the directory to temp folder incase any files are outputted
setwd("//192.168.2.32/Group/Data Team/Brand_Update_Location/13. FILE_DUMP")
	
##Create DCI Folder Path
DCIPath = paste("//192.168.2.32/GoogleDrive/FTP_Downloads/DCI_Files/",as.character(DCIFolderList), sep = "", collapse = NULL)
message("DCI path for brand is: ", DCIPath)

##Identify the Latest DCI file
LatestDCIFile = sort(list.files(DCIPath , pattern = "*.zip"),decreasing = TRUE)[1]
message("Latest DCI File is: ", LatestDCIFile )

##Create Path for latest File
LatestDCILocation = paste(as.character(DCIPath) ,as.character(LatestDCIFile), sep = "/", collapse = NULL)
message("Path of Latest DCI File is: ", LatestDCILocation )


#Find, upzip, remove duplicates and rbind to PooledPartsApp 
##find the Parts App file & Load it
PartsAppFile = paste(strsplit(LatestDCIFile , split='.zip', fixed=TRUE), "_Part_App.txt", sep = "",collapse = NULL)
DCIPartapp= read.table(unzip(LatestDCILocation ,PartsAppFile ), sep ="|", header = TRUE, dec =".", quote = "" , stringsAsFactors=T, fill = TRUE)

##Remove Duplicates
DCIPartapp$Duplicate = paste(DCIPartapp$exppartno,":",DCIPartapp$expldescr, sep = "", collapse = NULL)
DCIPartapp = DCIPartapp[!duplicated(DCIPartapp$Duplicate),]
	
##Add # Infront of Sku Parts App
DCIPartapp$Numb_Sku = paste("#",as.character(DCIPartapp$exppartno), sep = "", collapse = NULL)

##Stick DCIPartapp into PooledPartsApp 
PooledPartsApp = rbind(PooledPartsApp, DCIPartapp)

##Removed duplicates from across Part Apps
PooledPartsApp$Duplicate = paste(PooledPartsApp$exppartno,":",PooledPartsApp$expldescr, sep = "", collapse = NULL)
PooledPartsApp = PooledPartsApp[!duplicated(PooledPartsApp$Duplicate),]

##Trim the PooledPartsApp for necessary information
PooledPartsApp <- subset(PooledPartsApp, select = c("Numb_Sku", "expldescr","fnstring", "merchname","dciptdescr"))


message("Parts App Processed")


#Merge the PooledPartsApp with Update.PostIF.Merge to create Update.PostPA.Merge
Update.PostConversion.Merge = merge(Update.PostIF.Merge, PooledPartsApp,  by=c("Numb_Sku"), all = TRUE)
#Update.PostPA.Merge = merge(Update.PostConversion.Merge, REF.Conversion,  by=c("dciptdescr", "merchname"), all = TRUE)

Update.PostPA.Merge = Update.PostConversion.Merge

#Return the following data sets
return(list(PooledPartsApp, LatestDCIFile, LatestDCILocation, Update.PostPA.Merge ))


}


###########################################################
###########################################################
###########################################################
#TROUBLESHOOT

test = PartsApp(DCIFolderList = "MSD")
x = test[1]
TEMP.fileprint(x)



Update.PostIF.Merge
REF.Conversion
DCIFolderList


###########################################################
#Future Add Ons