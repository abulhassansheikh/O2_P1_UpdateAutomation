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
###Extract Formatted Jobber as UpdateFile
message("--------------------------*Formatted Jobber*")

#Load Formated Jobber as UpdateFile
BrandFile = paste(as.character(BrandName), "csv", sep = ".", collapse = NULL)
File_Location = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/1_Input_Folder", as.character(BrandFile), sep = "/", collapse = NULL)
UpdateFile=read.csv(File_Location, header = TRUE, row.names=NULL)

#Pull out Numb_Sku
UpdateFile$Numb_Sku<-as.character(UpdateFile$sku)
UpdateFileSku <- UpdateFile$Numb_Sku

message("Number of Skus on Jobber:", length(UpdateFileSku))


###Jobber Processing
#Remove number columns from jobber
numCols = c("usa_jobber_price", "ca_jobber_price", "Jobber_UPC", "Jobber_Weight", "Jobber_Length", "Jobber_Width", "Jobber_Height", "Numb_Sku")
jobber = UpdateFile[ , !(names(UpdateFile) %in% numCols )]

#Remove other # columns
for(i in 2:ncol(jobber)){	if(class(jobber[,i])== "integer"){numCols = c(numCols, names(jobber)[i])}	}
jobber = jobber[,!(names(jobber) %in% numCols )]

#Combin all text columns
for(i in 2:ncol(jobber)){jobber$Pro_String= paste(jobber$Pro_String,  trimws(jobber[,i]), sep=" ", collapse=NULL)}#jobber$sku,
string_jobber = subset(jobber, select=c(sku, Pro_String))
names(string_jobber) = c("Numb_Sku", "Pro_String")

message("Formatted Jobber Loaded")


###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT

UpdateFile_Output1 <- UpdateFile_Function (BrandName="MSD")
UpdateFile2 = UpdateFile_Output1[2]



###########################################################
#Future Add Ons