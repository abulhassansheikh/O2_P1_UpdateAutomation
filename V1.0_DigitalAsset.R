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
DigitalAsset <- function(ImageDecision, LatestDCIFile, LatestDCILocation){


message("--------------------------**")


PooledDigitalAsset = data.frame()

#Find, upzip and rbind to PooledDigitalAsset 
##Change the directory to temp folder incase any files are outputted
setwd("//192.168.2.32/Group/Data Team/Brand_Update_Location/13. FILE_DUMP")

##Find the Digital Asset file
DigitalAssetFile = paste(strsplit(LatestDCIFile , split='.zip', fixed=TRUE), "_DigitalAsset.txt", sep = "",collapse = NULL)
DCIDigitalAsset= read.table(unzip(LatestDCILocation ,DigitalAssetFile ), sep ="|", header = TRUE, dec =".", quote = "" , stringsAsFactors=T, fill = TRUE)

##Add # Infront of Sku Digital Asset
DCIDigitalAsset$Numb_Sku = paste("#",as.character(DCIDigitalAsset$exppartno), sep = "", collapse = NULL)

##Stick DCIDigitalAsset into PooledDigitalAsset 
PooledDigitalAsset = rbind(PooledDigitalAsset , DCIDigitalAsset)

message("Digital Asset Processed")


#set Image decision action to 0
ImageDecisionAction = 0

#Check if ImageDecision is 1
if(ImageDecision == 1){
	AFL = data.frame(as.character(list.files("//192.168.2.32/Group/Data Team/Brand_Update_Location/3_Image_Input_Folder")))
	names(AFL) = "Files"

#For all files within the Image input folder, check if brand name is in there, if so, change ImageDecisionAction to 1
	for(i in 1:nrow(AFL)){
		if(AFL[i,1] ==BrandName){
		message("Conducting Update WITH Images")
		ImageDecisionAction = 1
		}
	}
} else{message("Conducting Update WITHOUT Images")} 


message("Image Decision Complete")


#If ImageDecisionAction is 1, then do the following, which is to process the images 
if(ImageDecisionAction ==1){


message("Processing Images Now")


#Location of Brand Image Location
InputIMAGEFolder = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/3_Image_Input_Folder/", BrandName, sep = "", collapse = NULL)

#Pull files from Image folder
ExistingFiles = list.files(InputIMAGEFolder)
ExistingFiles <- data.frame(ExistingFiles )
REF.ExistingFiles <- data.frame(ExistingFiles )

#Label existing images with "Existing"
ExistingFiles$newcolumn<-"Exisiting"
names(ExistingFiles) <- c("filename", "FileStatus")

#Identify which I have from the folder vs Digital Assets
DAFiles <- merge(DCIDigitalAsset, ExistingFiles , by=c("filename")) 

#Create a reference SkuDocumentList df
SkuDocList <- subset(NewSkuDF, STATUS=="NEW" , select=c(Numb_Sku, STATUS))

#Identify New Sku Files that exist
NewFiles <- subset(merge(SkuDocList , DAFiles , by=c("Numb_Sku"), all=TRUE), STATUS=="NEW" & FileStatus == "Exisiting")

#Subset images and pdf
Images <- subset(NewFiles , style== "PIC")
PDF <- subset(NewFiles , style== "INS")

FileDF = data.frame()

#Identify the first four images/pdf per new sku and store them in respective column in SkuDocList 
for(i in 1:nrow(SkuDocList)){

	#Use the NewSku to subset the sorted image and pdf files
	ImageSubset = subset(Images , Images$Numb_Sku== as.character(SkuDocList$Numb_Sku[i]), select=(filename))

	#ImageSubset = subset(Images , Images$Numb_Sku== "#21-90101", select=(filename))
	#PDFSubset = subset(PDF , Images$Numb_Sku== "#21-901201", select=(filename))

	PDFSubset = subset(PDF , PDF$Numb_Sku== as.character(SkuDocList$Numb_Sku[i]), select=(filename))

	ImageSet <- data.frame(ImageSubset[order(ImageSubset$filename),]) 
	PDFSet <- data.frame(PDFSubset[order(PDFSubset$filename),])

	I1 = as.character(ImageSet[1,1])
	bI1= paste(Prefix.Image, tolower(I1),  sep = "-", collapse = NULL)
	OldFile1 = paste(InputIMAGEFolder, I1 ,  sep = "/", collapse = NULL)
	OUTPUTNewFile1 = paste(Brand_OutputIMAGELocation, bI1,  sep = "/", collapse = NULL)
	file.copy(OldFile1 , OUTPUTNewFile1 )
	BACKUPNewFile1 = paste(Brand_BackUpIMAGELocation, bI1,  sep = "/", collapse = NULL)
	file.copy(OldFile1 , BACKUPNewFile1 )


	I2 = as.character(ImageSet[2,1])
	bI2= paste(Prefix.Image, tolower(I2),  sep = "-", collapse = NULL)
	OldFile2 = paste(InputIMAGEFolder, I2 ,  sep = "/", collapse = NULL)
	OUTPUTNewFile2 = paste(Brand_OutputIMAGELocation, bI2,  sep = "/", collapse = NULL)
	file.copy(OldFile2 , OUTPUTNewFile2 )
	BACKUPNewFile2 = paste(Brand_BackUpIMAGELocation, bI2,  sep = "/", collapse = NULL)
	file.copy(OldFile2 , BACKUPNewFile2 )


	I3 = as.character(ImageSet[3,1])
	bI3= paste(Prefix.Image, tolower(I3),  sep = "-", collapse = NULL)
	OldFile3 = paste(InputIMAGEFolder, I3 ,  sep = "/", collapse = NULL)
	OUTPUTNewFile3 = paste(Brand_OutputIMAGELocation, bI3,  sep = "/", collapse = NULL)
	file.copy(OldFile3 , OUTPUTNewFile3 )
	BACKUPNewFile3 = paste(Brand_BackUpIMAGELocation, bI3,  sep = "/", collapse = NULL)
	file.copy(OldFile3 , BACKUPNewFile3 )

	I4 = as.character(ImageSet[4,1])
	bI4= paste(Prefix.Image, tolower(I4),  sep = "-", collapse = NULL)
	OldFile4 = paste(InputIMAGEFolder, I4 ,  sep = "/", collapse = NULL)
	OUTPUTNewFile4 = paste(Brand_OutputIMAGELocation, bI4,  sep = "/", collapse = NULL)
	file.copy(OldFile4 , OUTPUTNewFile4 )
	BACKUPNewFile4 = paste(Brand_BackUpIMAGELocation, bI4,  sep = "/", collapse = NULL)
	file.copy(OldFile4 , BACKUPNewFile4 )

	PDF1 = as.character(PDFSet [1,1])
	bPDF1= paste(Prefix.Image, tolower(PDF1) ,  sep = "-", collapse = NULL)
	OldFile5 = paste(InputIMAGEFolder, PDF1 ,  sep = "/", collapse = NULL)
	OUTPUTNewFile5 = paste(Brand_OutputIMAGELocation, bPDF1,  sep = "/", collapse = NULL)
	file.copy(OldFile5 , OUTPUTNewFile5 )
	BACKUPNewFile5 = paste(Brand_BackUpIMAGELocation, bPDF1,  sep = "/", collapse = NULL)
	file.copy(OldFile5 , BACKUPNewFile5 )

	PDF2 = as.character(PDFSet[2,1])
	bPDF2= paste(Prefix.Image, tolower(PDF2) ,  sep = "-", collapse = NULL)
	OldFile6 = paste(InputIMAGEFolder, PDF2 ,  sep = "/", collapse = NULL)
	OUTPUTNewFile6 = paste(Brand_OutputIMAGELocation, bPDF2,  sep = "/", collapse = NULL)
	file.copy(OldFile6 , OUTPUTNewFile6 )
	BACKUPNewFile6 = paste(Brand_BackUpIMAGELocation, bPDF2,  sep = "/", collapse = NULL)
	file.copy(OldFile6 , BACKUPNewFile6 )

	PDF3 = as.character(PDFSet[3,1])
	bPDF3= paste(Prefix.Image, tolower(PDF3) ,  sep = "-", collapse = NULL)
	OldFile7 = paste(InputIMAGEFolder, PDF3 ,  sep = "/", collapse = NULL)
	OUTPUTNewFile7 = paste(Brand_OutputIMAGELocation, bPDF3,  sep = "/", collapse = NULL)
	file.copy(OldFile7 , OUTPUTNewFile7 )
	BACKUPNewFile7 = paste(Brand_BackUpIMAGELocation, bPDF3,  sep = "/", collapse = NULL)
	file.copy(OldFile7 , BACKUPNewFile7 )

	PDF4 = as.character(PDFSet[4,1])
	bPDF4= paste(Prefix.Image, tolower(PDF4) ,  sep = "-", collapse = NULL)
	OldFile8 = paste(InputIMAGEFolder, PDF4 ,  sep = "/", collapse = NULL)
	OUTPUTNewFile8 = paste(Brand_OutputIMAGELocation, bPDF4,  sep = "/", collapse = NULL)
	file.copy(OldFile8 , OUTPUTNewFile8 )
	BACKUPNewFile8 = paste(Brand_BackUpIMAGELocation, bPDF4,  sep = "/", collapse = NULL)
	file.copy(OldFile8 , BACKUPNewFile8 )

	PooledDAFile = data.frame(t(c(as.character(SkuDocList$Numb_Sku[i]), bI1, bI2, bI3, bI4, bPDF1, bPDF2, bPDF3, bPDF4)))

	FileDF = rbind(FileDF , PooledDAFile )
}

names(FileDF) = c("Numb_Sku", "Image_One", "Image_Two", "Image_Three", "Image_Four", "PDF_One", "PDF_Two", "PDF_Three", "PDF_Four")

Update.PostPA.Merge = merge(Update.PostPA.Merge,FileDF,  by=c("Numb_Sku"), all = TRUE)


message("Image Process Complete")

}

#Return the following data sets
return(list(PooledDigitalAsset))


}


###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT

DigitalAsset(LatestDCIFile = "MSD", LatestDCILocation =  )


###########################################################
#Future Add Ons
