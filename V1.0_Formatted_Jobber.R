############################################################
#' A Formatted Jobber Extraction Function
#'
#' This function allows you to Extract the formatted jobber to be use for Update
#' @param Defaults to TRUE.
#' @keywords Formatted Jobber
#' @export
#' @examples
#' UpdateFile(BrandName="MSD")
###########################################################
UpdateFile_Function <- function(BrandName){


message("--------------------------*Formatted Jobber*")


#Load Formated Jobber as UpdateFile
BrandFile = paste(as.character(BrandName), "csv", sep = ".", collapse = NULL)
File_Location = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/1_Input_Folder", as.character(BrandFile), sep = "/", collapse = NULL)
UpdateFile=read.csv(File_Location, header = TRUE, row.names=NULL)


#Pull out Numb_Sku
UpdateFile$Numb_Sku<-as.character(UpdateFile$sku)
UpdateFileSku <- UpdateFile$Numb_Sku


message("Formatted Jobber Loaded")


#Return the following data sets
return(list(UpdateFile,UpdateFileSku))


}

###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT

UpdateFile_Output1 <- UpdateFile_Function (BrandName="MSD")
UpdateFile2 = UpdateFile_Output1[2]



###########################################################
#Future Add Ons