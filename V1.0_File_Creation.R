############################################################
#' A Folder/File creation function
#'
#' This function allows you to create a set of folders and files depending on the brand name what is required from the update.
#' @param Defaults to TRUE.
#' @keywords File/Folder Creation
#' @export
#' @examples
#' #Folder_File_Creation(BrandName="Test", Final1_Test0=0)
###########################################################
Folder_File_Creation<- function(BrandName, Final1_Test0, PulledMain, PulledIF, PooledPartsApp, PooledDigitalAsset, CompleteUpdate){


message("--------------------------*Folder_File_Creation*")


#Record Time of creation
##Create Folder Name with date and Time
BrandFolderName = paste(BrandName,gsub(":", "-", as.character(Sys.time())), sep = "-", collapse = NULL)
message("Folders & Files will be named: ", BrandFolderName)

#Load openxlsx package to save excel file with tabs
library("openxlsx")
##Create the workbook for excel and add the following tabs: "Requirements", "Internal_Audit", "Audit"
CompiledSheet <- createWorkbook()

addWorksheet(CompiledSheet , "New_Audit")
writeData(CompiledSheet , sheet = "New_Audit", x = read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/New_Audit.csv", header = TRUE, row.names=NULL))

addWorksheet(CompiledSheet , "Internal_Audit")
writeData(CompiledSheet , sheet = "Internal_Audit", x = read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/Internal_Audit.csv", header = TRUE, row.names=NULL))

addWorksheet(CompiledSheet , "NS--BRANDNAME.csv")
writeData(CompiledSheet , sheet = "NS--BRANDNAME.csv", x = read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/Consol_Ref_Sheet.csv", header = TRUE, row.names=NULL))

#Attach PulledMain to Compiled Sheet
addWorksheet(CompiledSheet , "Old_Main")
writeData(CompiledSheet , sheet = "Old_Main", x = PulledMain)


#Input, Backup and output folder names
InputFolder = "//192.168.2.32/Group/Data Team/Brand_Update_Location/1_Input_Folder"
BackUpFolder = "//192.168.2.32/Group/Data Team/Brand_Update_Location/1a_Input_Folder_BACKUP"
TEMPFolder = "//192.168.2.32/Group/Data Team/Brand_Update_Location/1b_Input_Folder_TEMP"
OutputFolder = "//192.168.2.32/Group/Data Team/Brand_Update_Location/2_Output_Folder"


#Check if FileOutput is 1
if(Final1_Test0== 1){


#Make Output & Backup Folder
##----------------BackUP Folder------
###Parent BackUp Folder
Brand_BackUpLocation = paste(BackUpFolder,BrandFolderName, sep = "/", collapse = NULL)
	dir.create(paste(BackUpFolder,BrandFolderName, sep = "/", collapse = NULL)) 

###Child Backup Image Folder
Brand_BackUpIMAGELocation = paste(BackUpFolder,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL)
	dir.create(paste(BackUpFolder,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL)) 

###Child Backup Reference Folder
Brand_BackUpREFERENCELocation = paste(BackUpFolder,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL)
	dir.create(paste(BackUpFolder,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL)) 

##----------------Output Folder------
###Parent Output Folder
Brand_OutputLocation = paste(OutputFolder,BrandFolderName, sep = "/", collapse = NULL)
	dir.create(paste(OutputFolder,BrandFolderName , sep = "/", collapse = NULL))

###Child Output Image Folder
Brand_OutputIMAGELocation = paste(OutputFolder,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL)
	dir.create(paste(OutputFolder,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL))

###Child Output Reference Folder
Brand_OutputREFERENCELocation = paste(OutputFolder,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL)
	dir.create(paste(OutputFolder,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL))

message("Output & Backup Folders Created")

##----------Reference File Output---------
###Output the mainsheet into child reference folders
MSFileName = paste("MainSheet" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PulledMain, file = paste(Brand_BackUpREFERENCELocation, MSFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)
write.csv(PulledMain, file = paste(Brand_OutputREFERENCELocation, MSFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

###Output the PulledIF  into child reference folders
PulledIF_Name = paste("InventoryFile" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PulledIF , file = paste(Brand_BackUpREFERENCELocation, PulledIF_Name, sep = "/", collapse = NULL) , na="", row.names=FALSE)
write.csv(PulledIF , file = paste(Brand_OutputREFERENCELocation, PulledIF_Name , sep = "/", collapse = NULL) , na="", row.names=FALSE)

###Output the PooledPartsApp into child reference folders
PAFileName = paste("PartsApp " ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PooledPartsApp , file = paste(Brand_BackUpREFERENCELocation, PAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)
write.csv(PooledPartsApp , file = paste(Brand_OutputREFERENCELocation, PAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

###Output the PooledDigitalAsset into child reference folders
DAFileName = paste("DigitalAsset " ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PooledDigitalAsset , file = paste(Brand_BackUpREFERENCELocation, DAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)
write.csv(PooledDigitalAsset , file = paste(Brand_OutputREFERENCELocation, DAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

UDFileName = paste("UpdateFile" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(CompleteUpdate , file = paste(Brand_BackUpREFERENCELocation, UDFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)
write.csv(CompleteUpdate , file = paste(Brand_OutputREFERENCELocation, UDFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)


##-----------Create CompiledSheet------------
###Attach Updatefile to CompiledSheet
addWorksheet(CompiledSheet , "Formated_Jobber")
writeData(CompiledSheet, sheet = "Formated_Jobber", x = UpdateFile)

###Attach CompleteUpdate to CompiledSheet
addWorksheet(CompiledSheet , "Update_Analysis")
writeData(CompiledSheet , sheet = "Update_Analysis", x = CompleteUpdate)

###Attach Upfate.PostIF.Merge with CompiledSheet
addWorksheet(CompiledSheet , "Inventory_File")
writeData(CompiledSheet , sheet = "Inventory_File", x = PulledIF)

###Attach PooledPartsApp to CompiledSheet
addWorksheet(CompiledSheet , "Parts_App")
writeData(CompiledSheet , sheet = "Parts_App", x = PooledPartsApp)

###Attach DAFileName to CompiledSheet
addWorksheet(CompiledSheet , "Digital_Asset")
writeData(CompiledSheet , sheet = "Digital_Asset", x = PooledDigitalAsset)

saveWorkbook(CompiledSheet, paste(Brand_OutputLocation, UDFileName , sep = "/", collapse = NULL), overwrite = FALSE)
saveWorkbook(CompiledSheet, paste(Brand_BackUpLocation, UDFileName , sep = "/", collapse = NULL), overwrite = FALSE)

} else{


#Make Temp Folder
##----------------TEMP Folder------
#Parent Output Folder
Brand_TEMPLocation = paste(TEMPFolder ,BrandFolderName, sep = "/", collapse = NULL)
	dir.create(paste(TEMPFolder ,BrandFolderName , sep = "/", collapse = NULL))

###Child Output Image Folder
Brand_TEMPIMAGELocation = paste(TEMPFolder ,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL)
	dir.create(paste(TEMPFolder ,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL))

###Child Output Reference Folder
Brand_TempREFERENCELocation = paste(TEMPFolder ,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL)
	dir.create(paste(TEMPFolder ,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL))

message("TEMP Folders Created")

##----------Reference File Output---------
###Output the mainsheet into child reference folders
MSFileName = paste("MainSheet" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PulledMain, file = paste(Brand_TempREFERENCELocation, MSFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

###Output the PulledIF  into child reference folders
PulledIF_Name = paste("InventoryFile" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PulledIF , file = paste(Brand_TempREFERENCELocation, PulledIF_Name, sep = "/", collapse = NULL) , na="", row.names=FALSE)

###Output the PooledPartsApp into child reference folders
PAFileName = paste("PartsApp " ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PooledPartsApp , file = paste(Brand_TempREFERENCELocation, PAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

###Output the PooledDigitalAsset into child reference folders
DAFileName = paste("DigitalAsset " ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PooledDigitalAsset , file = paste(Brand_TempREFERENCELocation, DAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

UDFileName = paste("UpdateFile" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(CompleteUpdate , file = paste(Brand_TempREFERENCELocation, UDFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

##-----------Create CompiledSheet------------
###Attach Updatefile to CompiledSheet
addWorksheet(CompiledSheet , "Formated_Jobber")
writeData(CompiledSheet, sheet = "Formated_Jobber", x = UpdateFile)

###Attach CompleteUpdate to CompiledSheet
addWorksheet(CompiledSheet , "Update_Analysis")
writeData(CompiledSheet , sheet = "Update_Analysis", x = CompleteUpdate)

###Attach Upfate.PostIF.Merge with CompiledSheet
addWorksheet(CompiledSheet , "Inventory_File")
writeData(CompiledSheet , sheet = "Inventory_File", x = PulledIF)

###Attach PooledPartsApp to CompiledSheet
addWorksheet(CompiledSheet , "Parts_App")
writeData(CompiledSheet , sheet = "Parts_App", x = PooledPartsApp)

###Attach DAFileName to CompiledSheet
addWorksheet(CompiledSheet , "Digital_Asset")
writeData(CompiledSheet , sheet = "Digital_Asset", x = PooledDigitalAsset)

saveWorkbook(CompiledSheet, paste(Brand_TEMPLocation, UDFileName , sep = "/", collapse = NULL), overwrite = FALSE)


} 


}


############################################
############################################
############################################
############################################
#TROUBLESHOOT
#PulledMain = data.frame()
#PulledIF = data.frame()
#PooledPartsApp = data.frame()
#PooledDigitalAsset = data.frame()
#CompleteUpdate = data.frame()
#UpdateFile = data.frame()



#Folder_File_Creation(BrandName="Test", Final1_Test0=0)
Folder_File_Creation(BrandName = BrandName, Final1_Test0 = Final1_Test0, PulledMain = data.frame(main_sheet_Output[1]))



###########################################################
#Future Add Ons







































