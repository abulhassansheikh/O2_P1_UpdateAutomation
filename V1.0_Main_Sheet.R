############################################################
#' A Main Sheet extracting Function
#'
#' This function allows you to ID and pull out main sheet
#' @param Defaults to TRUE.
#' @keywords MainSheet
#' @export
#' @examples
#' main_sheet(BrandName)
###########################################################
main_sheet <- function(BrandName){


#Extract the Main sheet
message("--------------------------*main_sheet*")


#Create Mainsheet location path
BrandFolderLocation = paste("//192.168.2.32/GoogleDrive/Completed Magento Uploads (v 1.0)/",as.character(BrandName), sep = "", collapse = NULL)
message("Path of Mainsheet: ", BrandFolderLocation )


#Go to the BrandFolderLocation  location
setwd(BrandFolderLocation)


#Identify the Main--sheet and pull it
PulledMain=read.csv(Sys.glob("main--*.csv"), header = TRUE)


#Pull out skus
PulledMain$sku <- as.character(PulledMain$sku)
MainSku<-data.frame(PulledMain$sku)
names(MainSku) = "MainSku"
	

#Pull out any non-discontinued skus
UnDeletedSKU <- subset(PulledMain, delete=="N" & PulledMain$type=="simple", select=(sku))
UnDeletedSKU <- as.character(UnDeletedSKU$sku)


message("MainSheet Created")


#Return PulledMain, MainSku for use by other functions
return(list(PulledMain, MainSku, UnDeletedSKU))


}


###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT

main_sheet_Output<- main_sheet(BrandName="MSD")
#Test = data.frame(main_sheet_Output[2])
#names(Test)

#TEMP.fileprint(main_sheet_Output)




###########################################################
#Create a match sheet from this data to place on Match tab on compiled sheet

