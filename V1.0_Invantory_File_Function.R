############################################################
#' A Invantory File merge Function
#'
#' This function allows you to merge multiple invantory files with the simple update
#' @param Defaults to TRUE.
#' @keywords Merge Invatory File
#' @export
#' @examples
#' UsesIF()
###########################################################
#Use the preloaded inventory file
message("--------------------------*Invantory File*")


#All Possible Inventory Files
vendor_name_1 = as.character(REF.BrandData$vendor_name_1)
vendor_name_2 = as.character(REF.BrandData$vendor_name_2)
vendor_name_3 = as.character(REF.BrandData$vendor_name_3)
message("Using Invantory File Vendor Names: ", vendor_name_1, ", ", vendor_name_2, ", ",vendor_name_3  )


#Subset Invantory File and and combin multiple vendor names into the PulledIF df
IFV1 = subset(InvantoryFile, vendor_name == vendor_name_1)
IFV2 = subset(InvantoryFile, vendor_name == vendor_name_2)
IFV3 = subset(InvantoryFile, vendor_name == vendor_name_3)


#Combin the different vendot names into one file
PulledIF = data.frame()
PulledIF = rbind(PulledIF,IFV1)
PulledIF = rbind(PulledIF,IFV2)
PulledIF = rbind(PulledIF, IFV3)


#Subset the pulledIF by the folling columns
PulledIF <- subset(PulledIF , select = c("Numb_Sku", "price","TotalQty", "CaseQty","Weight","Height",
			"Length","Width","product_name", "upc"))

#Merge the final pulledIF with the simple match
Update.PostIF.Merge = merge(UpdatedSkuListSimple, PulledIF,  by="Numb_Sku", all = TRUE)


###IF Processing
#Remove number columns from jobber
string_IF <- subset(PulledIF , select = c("Numb_Sku", "product_name"))
names(string_IF) =c("Numb_Sku", "Pro_String")


message("Invantory File Created")


###########################################################
#TROUBLESHOOT
invantoryfile()

###########################################################
#Future Add Ons