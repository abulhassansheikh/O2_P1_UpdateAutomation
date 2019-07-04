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
#Find and extract Parts app and merge it with Update.PostIF.Merge
message("--------------------------*Pooled Parts App *")

#Find the latest file within the specific DCI folder
##Change the directory to temp folder incase any files are outputted
setwd("//192.168.2.32/Group/Data Team/Brand_Update_Location/13. FILE_DUMP")


if(DCIFolderList != "DUMB"){

#Create empty Data frame to collect the multiple DCI folder data
PooledPartsApp = data.frame()

##Create DCI Folder Path
DCIPath = paste("//192.168.2.32/GoogleDrive/FTP_Downloads/DCI_Files/",as.character(DCIFolderList), sep = "", collapse = NULL)
message("DCI path for brand is: ", DCIPath)


##Identify the Latest DCI file
LatestDCIFile = sort(list.files(DCIPath , pattern = "*.zip"),decreasing = TRUE)[1]
message("Latest DCI File is: ", LatestDCIFile )

##Create Path for latest File
LatestDCILocation = paste(as.character(DCIPath) ,as.character(LatestDCIFile), sep = "/", collapse = NULL)
message("Path of Latest DCI File is: ", LatestDCILocation )

if(DCIFolderList != "DUMB"){

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
#DropDCIcols = c("Duplicate")
#PooledPartsApp[,!(names(PooledPartsApp) %in% DropDCIcols)]
PooledPartsApp <- subset(PooledPartsApp, select = c("Numb_Sku", "expldescr","fnstring", "merchname","dciptdescr"))
message("Number of Unique Skus on Parts App File: ", nrow(PooledPartsApp))


#Merge the PooledPartsApp with Update.PostIF.Merge to create Update.PostPA.Merge
Update.PostConversion.Merge = merge(Update.PostIF.Merge, PooledPartsApp,  by=c("Numb_Sku"), all = TRUE)
#Update.PostPA.Merge = merge(Update.PostConversion.Merge, REF.Conversion,  by=c("dciptdescr", "merchname"), all = TRUE)

Update.PostPA.Merge = Update.PostConversion.Merge

###Process Parts app for Prediction
PooledPartsApp$Pro_String = paste(PooledPartsApp$expldescr, PooledPartsApp$merchname , PooledPartsApp$dciptdescr , sep=" ")
string_DCI = subset(PooledPartsApp, select = c("Numb_Sku", "Pro_String")) 

} else {Update.PostPA.Merge  = Update.PostIF.Merge; string_DCI = "FALSE" }

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