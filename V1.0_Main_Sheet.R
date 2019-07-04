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
###Extract the Main sheet
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

message("Total Skus on MainSheet:", length(MainSku))

#Pull out any non-discontinued skus
UnDeletedSKU <- subset(PulledMain, delete=="N" & PulledMain$type=="simple", select=(sku))
UnDeletedSKU <- as.character(UnDeletedSKU$sku)

message("Number of ACTIVE Skus on MainSheet:", length(UnDeletedSKU))


###Mainsheet Processing
#Pull out any non-discontinued skus for Part type
MS_PTlabel <- subset(PulledMain, PulledMain$type=="simple" & PulledMain$part_type_filter!="" & PulledMain$delete == "N", select=c(sku, part_type_filter))
names(MS_PTlabel) = c("Numb_Sku", "Pro_Label")

#Pull out any non-discontinued skus for Series
MS_Serieslabel <- subset(PulledMain, PulledMain$type=="simple" & PulledMain$part_type_filter!="" & PulledMain$delete == "N", select=c(sku, series_parent))
names(MS_Serieslabel) = c("Numb_Sku", "Pro_Label")

message("MainSheet Created")


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

