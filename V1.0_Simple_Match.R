############################################################
#' A Simple Match Function
#'
#' This function allows you to conduct a simple Match
#' @param Defaults to TRUE.
#' @keywords Simple Match
#' @export
#' @examples
#' simple_match()
###########################################################
simple_match <- function(MainSku, UnDeletedSKU, UpdateFileSku, UpdateFile, attribute_set, Prefix.InSku){
  
  message("--------------------------*Simple Match*")
  
  #Find Set Difference
  REF.NewSkuList = setdiff(UpdateFileSku, MainSku)
  REF.DiscontSkuList = setdiff(UnDeletedSKU , UpdateFileSku) 
  
  if(identical(REF.NewSkuList, character(0)) == FALSE){
    #Make NewSkuDF which has only new skus with the status "New" and are Revised
    NewSkuDF = data.frame(REF.NewSkuList)
    NewSkuDF$STATUS <- "NEW"
    NewSkuDF$REVISED <- "Revised"
    names(NewSkuDF) <- c("Numb_Sku", "STATUS", "RevisedSku")
    
  } else{
    #create a filler value
    REF.NewSkuList = "#NoNewSku"
    
    #Make NewSkuDF which has only new skus with the status "New" and are Revised
    NewSkuDF = data.frame(REF.NewSkuList)
    NewSkuDF$STATUS <- "NEW"
    NewSkuDF$REVISED <- "Revised"
    names(NewSkuDF) <- c("Numb_Sku", "STATUS", "RevisedSku")
  }
  
  if(identical(REF.DiscontSkuList, character(0)) == FALSE){
    #Make DList Dataframe with all the discontinued skus with discontinued status and revised
    DList <- data.frame(REF.DiscontSkuList)
    DList$STATUS <- "Discontinued"
    DList$REVISED <- "Revised"
    names(DList) <- c("Numb_Sku", "STATUS", "RevisedSku")
  } else{
    #create a filler value
    REF.DiscontSkuList = "#NoDiscontinuedSku"
    
    #Make DList Dataframe with all the discontinued skus with discontinued status and revised
    DList <- data.frame(REF.DiscontSkuList)
    DList$STATUS <- "Discontinued"
    DList$REVISED <- "Revised"
    names(DList) <- c("Numb_Sku", "STATUS", "RevisedSku")
  }
  
  #Make NewSkuWdata Dataframe which has the updateFile data included in it
  NewSkuWdata <- merge(UpdateFile, NewSkuDF, by="Numb_Sku")
  
  #Make UpdatedSkuListSimple Dataframe which has both the new, discontinued skus with the updatedFile data
  UpdatedSkuListSimple <- merge(NewSkuWdata , DList, by=c("Numb_Sku","STATUS", "RevisedSku"), all = TRUE)
  UpdatedSkuListSimple$attribute_set <- attribute_set
  UpdatedSkuListSimple$Internal_Sku <- paste(Prefix.InSku, gsub("\\#", "", UpdatedSkuListSimple$Numb_Sku), sep = "-", collapse = NULL)
  
  message("Simple Match Completed")
  
  
  
  #Return the following data sets
  return(list(UpdatedSkuListSimple))
  
}


###########################################################
#TROUBLESHOOT


###########################################################
#Future Add Ons





