############################################################
#' A XXX Function
#'
#' This function allows you to XXX
#' @param Defaults to TRUE.
#' @keywords XXX
#' @export
#' @examples
#' XXX()
###########################################################
FileRestructure <- function(Update.PostPA.Merge){

message("--------------------------*FileRestructure*")


#Subset off revised skus
FinalUpdateFile = subset(Update.PostPA.Merge, RevisedSku =="Revised")

#File Restructure
JobberDescription <- subset(FinalUpdateFile , select = -c(

sku, price, TotalQty, CaseQty, Weight, Height, Length, Width, product_name, upc, 

expldescr, fnstring, merchname, dciptdescr, part_type_filter, series_parent,Category_Location,

Numb_Sku, Internal_Sku, STATUS, RevisedSku, attribute_set,

usa_jobber_price, ca_jobber_price, Jobber_UPC, Jobber_Weight, Jobber_Length, Jobber_Width, Jobber_Height

))

AllSkuInformation <- subset(FinalUpdateFile , select = c(
#Base Data
Numb_Sku, Internal_Sku, STATUS, RevisedSku, attribute_set,

#Pricing Data
usa_jobber_price, ca_jobber_price, price,

#Product Dimentions
Jobber_Weight, Weight,
Jobber_Height, Height, 
Jobber_Length, Length, 
Jobber_Width, Width, 

upc, Jobber_UPC,

TotalQty, CaseQty, 

#Product Description Data
dciptdescr, merchname, Category_Location, part_type_filter, series_parent, expldescr, product_name 

))

#fnstring
#Make the final Update File
CompleteUpdate <- cbind(AllSkuInformation, JobberDescription)



#Return the following data sets
return(list(CompleteUpdate))

}

###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT


###########################################################
#Future Add Ons

