############################################################
#' A Brand Prefix labeling Function
#'
#' This function allows you to create all brand prefix depending on the BrandName
#' @param Defaults to TRUE.
#' @keywords Prefix Labeling
#' @export
#' @examples
#' Prefix(BrandName="MSD")
###########################################################
Prefix <- function(BrandName){


message("--------------------------*Prefix Function*")


#Loaded prefix file
REF.Prefix=read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/Brands_Prefix.csv", header = TRUE, row.names=NULL)


#Create REF.BrandData df, which contains all the prefix information for the BrandName Value
REF.BrandData = data.frame(subset(REF.Prefix, Brand_Folder_Name == as.character(BrandName)))
	#print(REF.BrandData)

#Extract all information about brand from Prefix file in order(B1S)
BrandName = as.character(REF.BrandData$Brand_Folder_Name)
	#print(BrandName)
	
attribute_set = as.character(REF.BrandData$attribute_set)
message("Brand Attribute Set: ", attribute_set)
	
Prefix.InSku = as.character(REF.BrandData$Internal.SKU.Prefix)
message("Brand Internal Sku Prefix: ", Prefix.InSku)

Prefix.Cat = as.character(REF.BrandData$Category.Brand.Name)
message("Brand Category Prefix: ", Prefix.Cat)
	
Prefix.Image = as.character(REF.BrandData$Image.Prefix)
message("Brand Image Prefix: ", Prefix.Image)


#Determine the DCI fildername
DCIFolderList = as.character(REF.BrandData$DCI_Internal_1)
message("Using DCI Folder: ", DCIFolderList)


#Return the following data sets
return(list(BrandName ,attribute_set, Prefix.InSku, Prefix.Cat , Prefix.Image, REF.BrandData, DCIFolderList ))


}

###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT
Prefix_Outout = Prefix(BrandName="MSD")
DCIFolderList <- as.character(Prefix_Outout[7])


###########################################################
#Future Add Ons






