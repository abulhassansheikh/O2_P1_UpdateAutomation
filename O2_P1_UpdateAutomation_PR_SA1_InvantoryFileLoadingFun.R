############################################################
#' A Inventory File Loading Function
#'
#' This function allows you to Create the InvantoryFile Dataframe using the function: "LoadLatestInventoryFile()"
#' @param Defaults to TRUE.
#' @keywords Invantory File
#' @export
#' @examples
#' GetLatestInventoryFile ()
###########################################################
GetLatestInventoryFile <- function(){


message("--------------------------*Loading Latest Inventory File*")


#Identify The latest Invantory file based on file name
LatestIFFile = sort(list.files("//192.168.2.32/GoogleDrive/FTP_Downloads/Inventory_File" , pattern = "*.ZIP"),decreasing = TRUE)[1]
message("Identifying Latest Invantory File")


#Paste Location after identifying latest Invantory File
IFLocation = paste("//192.168.2.32/GoogleDrive/FTP_Downloads/Inventory_File",as.character(LatestIFFile) , sep = "/", collapse = NULL)


#Identify Exact Invantory File from Ziped folder as IFFile 
IFFile = grep('\\.csv$', unzip(IFLocation , list=TRUE)$Name, ignore.case=TRUE, value=TRUE)
message("Latest Invantory File is: ", IFFile)


#pull Unziped Invantory File and place into InvantoryFile df
message("Loading Invantory File, this may take some time...")
InvantoryFile = read.csv(unzip(IFLocation ,IFFile ), header = TRUE, row.names=NULL)


#Remove the "=" from infront of skus
InvantoryFile$mpn= substring(InvantoryFile$mpn, 2)


#Place a "#" infront of sku and turn into a character. 
InvantoryFile$Numb_Sku = paste("#",as.character(InvantoryFile$mpn), sep = "", collapse = NULL)


message("Latest InvantoryFile ready to use!")


#Return the following data sets
return(InvantoryFile)


}


###########################################################
#TROUBLESHOOT

InvantoryFile = GetLatestInventoryFile()
InvantoryFile = data.frame(InvantoryFile)

###########################################################
#Future Add Ons














