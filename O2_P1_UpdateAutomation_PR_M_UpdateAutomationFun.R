#Within the //192.168.2.32/Group/Data Team/Brand_Update_Location/1_Input_Folder location, look at each file and check if it is on prefix
#File, if it is, then conduct update, if it is not, then move to next file. 


############################################################ #(A1S)Create Update.Brand Function (A1S)
Update.Brand <- function(BrandName,ImageDecision, Final1_Test0){

Update.Brand(BrandName = "", ImageDecision = "0", Final1_Test0 = "0")
BrandName = ""

##Record Time of creation
#Create Folder Name with date and Time
BrandFolderName = paste(BrandName,gsub(":", "-", as.character(Sys.time())), sep = "-", collapse = NULL)

############################################################ # Determine if Images will or can be processed
#Determine if Images will be Processed also
#ImageDecision <- readline("Are the Images within <Brand_Update_Location\3_Image_Input_Folder> Location? (1 = Yes, 0 = No):   ")  

#set Image decision action to 0
ImageDecisionAction = 0

#Check if ImageDecision is 1
if(ImageDecision == 1){
	AFL = data.frame(as.character(list.files("//192.168.2.32/Group/Data Team/Brand_Update_Location/3_Image_Input_Folder")))
	names(AFL) = "Files"

#For all files within the Image input folder, check if brand name is in there, if so, change ImageDecisionAction to 1
	for(i in 1:nrow(AFL)){
		if(AFL[i,1] ==BrandName){
		print("Conducting Update WITH Images")
		ImageDecisionAction = 1
		}
	}
} else{print("Conducting Update WITHOUT Images")} 


print("Image Decision Complete")

############################################################ # PreRequsit for Outputting single Xlsx file
library("openxlsx")

CompiledSheet <- createWorkbook()

addWorksheet(CompiledSheet , " Requirements")
writeData(CompiledSheet , sheet = " Requirements", x = read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/ Requirements_Instructions.csv", header = TRUE, row.names=NULL))

addWorksheet(CompiledSheet , "Internal_Audit")
writeData(CompiledSheet , sheet = "Internal_Audit", x = read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/Internal_Audit.csv", header = TRUE, row.names=NULL))

addWorksheet(CompiledSheet , " Audit")
writeData(CompiledSheet , sheet = " Audit", x = read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/ Audit.csv", header = TRUE, row.names=NULL))

############################################################ #Pre Loop file analysis and subseting prefix file depending on file names
#Loaded prefix file
REF.Prefix=read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/Brands_Prefix.csv", header = TRUE, row.names=NULL)

#Create REF.BrandData df, which contains all the prefix information for the BrandName Value
REF.BrandData = data.frame(subset(REF.Prefix, Brand_Folder_Name == as.character(BrandName)))

############################################################ #(B1S)Extract all information about brand from Prefix file in order(B1S)
BrandName = as.character(REF.BrandData$Brand_Folder_Name)
	#print(BrandName )
	
attribute_set = as.character(REF.BrandData$attribute_set)
	#print(attribute_set)
	
Prefix.InSku = as.character(REF.BrandData$Internal.SKU.Prefix)
	#print(Prefix.InSku)

Prefix.Cat = as.character(REF.BrandData$Category.Brand.Name)
	#print(Prefix.Cat)
	
Prefix.Image = as.character(REF.BrandData$Image.Prefix)
	#print(Prefix.Image)

DCIFolderList = as.character(REF.BrandData$DCI_Internal_1)

############################# #Loaded PartType-Series Conversion file
#Load Ref.Conversion_Raw File
PT = read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/Category_To_Parttype.csv", header = TRUE, row.names=NULL)

for(i in 1:nrow(PT)){

	L1_A_Blank = identical(as.character(PT[i,]$L1), "Blank")
	L1_A_Space = identical(as.character(PT[i,]$L1), "")
	L2_A_Blank = identical(as.character(PT[i,]$L2), "Blank")
	L2_A_Space = identical(as.character(PT[i,]$L2), "")
	L3_A_Blank = identical(as.character(PT[i,]$L3), "Blank")
	L3_A_Space = identical(as.character(PT[i,]$L3), "")
	L3_A_Empty = identical(as.character(PT[i,]$L3), "*Empty*")

	       if(L1_A_Blank == "TRUE" & L2_A_Blank ==  "TRUE" & L3_A_Blank ==  "TRUE"){
		  	PT[i,]$Category_Location= "All levels of Categories are missing for this Part-type"

	} else if(L1_A_Space == "TRUE"      | L2_A_Space == "TRUE"       | L3_A_Space == "TRUE"      ){
			PT[i,]$Category_Location= "One or more Category is missing for this Part-type in the 'Category_To_Parttype' file."

	} else if(L1_A_Blank == "FALSE" & L2_A_Blank  ==  "FALSE" & L3_A_Empty ==  "TRUE"){
			Cat_Raw_A = paste(PT[i,]$L1, PT[i,]$L2, sep = "/", collapse = NULL)
			Cat_Brand_A = paste("Brands",Prefix.Cat, PT[i,]$L2, sep = "/", collapse = NULL)
			CL = paste(Cat_Brand_A, Cat_Raw_A, sep = ";", collapse = NULL)
			PT[i,]$Category_Location= CL 

	} else{
			Cat_Raw_A = paste(PT[i,]$L1, PT[i,]$L2, PT[i,]$L3, sep = "/", collapse = NULL)
			Cat_Brand_A = paste("Brands",Prefix.Cat, PT[i,]$L2, PT[i,]$L3, sep = "/", collapse = NULL)
			CL = paste(Cat_Brand_A, Cat_Raw_A, sep = ";", collapse = NULL)
			PT[i,]$Category_Location= CL
	}
}




REF.Conversion_Raw = read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/DCI_to_Custom_PartType_and_Series.csv", header = TRUE, row.names=NULL)

for(i in 1:nrow(REF.Conversion_Raw)){

	PT2 = identical(as.character(REF.Conversion_Raw[i,]$part_type_filter_B),"None")
	PT3 = identical(as.character(REF.Conversion_Raw[i,]$part_type_filter_C),"None")


if(PT2 =="TRUE" & PT3 =="TRUE"){
#Below Has ONE part type
	REF.Conversion_Raw[i,]$part_type_filter =  as.character(REF.Conversion_Raw[i,]$part_type_filter_A)
	REF.Conversion_Raw[i,]$series_parent = 	as.character(REF.Conversion_Raw[i,]$series_parent_A)

	       if(L1_A_Blank == "TRUE" & L2_A_Blank ==  "TRUE" & L3_A_Blank ==  "TRUE"){
		  	REF.Conversion_Raw[i,]$Category_Location= "All levels of Categories are missing for this Part-type"

	} else if(L1_A_Space == "TRUE"      | L2_A_Space == "TRUE"       | L3_A_Space == "TRUE"      ){
			REF.Conversion_Raw[i,]$Category_Location= "One or more Category is missing for this Part-type in the 'Category_To_Parttype' file."

	} else if(L1_A_Blank == "FALSE" & L2_A_Blank  ==  "FALSE" & L3_A_Empty ==  "TRUE"){
			Cat_Raw_A = paste(PT[i,]$L1, PT[i,]$L2, sep = "/", collapse = NULL)
			Cat_Brand_A = paste("Brands",Prefix.Cat, PT[i,]$L2, sep = "/", collapse = NULL)
			CL = paste(Cat_Brand_A, Cat_Raw_A, sep = ";", collapse = NULL)
			REF.Conversion_Raw[i,]$Category_Location= CL 

	} else{
			Cat_Raw_A = paste(PT[i,]$L1, PT[i,]$L2, PT[i,]$L3, sep = "/", collapse = NULL)
			Cat_Brand_A = paste("Brands",Prefix.Cat, PT[i,]$L2, PT[i,]$L3, sep = "/", collapse = NULL)
			CL = paste(Cat_Brand_A, Cat_Raw_A, sep = ";", collapse = NULL)
			REF.Conversion_Raw[i,]$Category_Location= CL

	}


} else if(PT2 == "FALSE" & PT3 == "TRUE"){
#Below has TWO part types
	REF.Conversion_Raw[i,]$part_type_filter = paste(REF.Conversion_Raw[i,]$part_type_filter_A, REF.Conversion_Raw[i,]$part_type_filter_B, sep = ";", collapse = NULL)
	REF.Conversion_Raw[i,]$series_parent = 	paste(REF.Conversion_Raw[i,]$series_parent_A, REF.Conversion_Raw[i,]$series_parent_B, sep = ";", collapse = NULL)

	#Part Type Test
	PTTest_A = subset(REF.Cat2PT,Part.Type== as.character(REF.Conversion_Raw[i,]$part_type_filter_A),   select = c(L1, L2, L3))
	PTTest_B = subset(REF.Cat2PT,Part.Type== as.character(REF.Conversion_Raw[i,]$part_type_filter_B),   select = c(L1, L2, L3))


} else if(PT2 == "FALSE" & PT3 == "FALSE"){
#Below has THREE part types
	REF.Conversion_Raw[i,]$part_type_filter = paste(REF.Conversion_Raw[i,]$part_type_filter_A, REF.Conversion_Raw[i,]$part_type_filter_B, REF.Conversion_Raw[i,]$part_type_filter_C, sep = ";", collapse = NULL)
	REF.Conversion_Raw[i,]$series_parent = 	paste(REF.Conversion_Raw[i,]$series_parent_A, REF.Conversion_Raw[i,]$series_parent_B, REF.Conversion_Raw[i,]$series_parent_C, sep = ";", collapse = NULL)

	#Part Type Test
	PTTest_A = subset(REF.Cat2PT,Part.Type== as.character(REF.Conversion_Raw[i,]$part_type_filter_A),   select = c(L1, L2, L3))	
	PTTest_B = subset(REF.Cat2PT,Part.Type== as.character(REF.Conversion_Raw[i,]$part_type_filter_B),   select = c(L1, L2, L3))
	PTTest_C = subset(REF.Cat2PT,Part.Type== as.character(REF.Conversion_Raw[i,]$part_type_filter_C),   select = c(L1, L2, L3))
}

	#Sys.sleep(0.1)
	#setTxtProgressBar(pb, i)
}

TEMP.fileprint(REF.Conversion_Raw)


#Subset all necessary parts needed for updates
REF.Conversion = subset(REF.Conversion_Raw, select= c(dciptdescr, merchname, Category_Location, part_type_filter, series_parent))

REF.Conversion = REF.Conversion_Raw

############################# #Loaded PartType-Series Conversion file


#All Possible Inventory Files
vendor_name_1 = as.character(REF.BrandData$vendor_name_1)
vendor_name_2 = as.character(REF.BrandData$vendor_name_2)
vendor_name_3 = as.character(REF.BrandData$vendor_name_3)

#Load Formated Jobber as UpdateFile
BrandFile = paste(as.character(BrandName), "csv", sep = ".", collapse = NULL)
File_Location = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/1_Input_Folder", as.character(BrandFile), sep = "/", collapse = NULL)
UpdateFile=read.csv(File_Location, header = TRUE, row.names=NULL)

#Pull out Numb_Sku
UpdateFile$Numb_Sku<-as.character(UpdateFile$sku)
UpdateFileSku <- UpdateFile$Numb_Sku

print("References Created")

############################################################ #(B1E)Extract all information about brand from Prefix file(B1E)
############################################################ #(C1S)Extract Main sheet & Conduct Match and output ???? (C1S)
#=========================================================== #(C1AS)Extract the Main sheet(C1AS)
#Create Mainsheet location path
BrandFolderLocation = paste("//192.168.2.32/GoogleDrive/Completed Magento Uploads (v 1.0)/",as.character(BrandName), sep = "", collapse = NULL)

#Go to the BrandFolderLocation  location
setwd(BrandFolderLocation)

#Identify the Main--sheet and pull it
PulledMain=read.csv(Sys.glob("main--*.csv"), header = TRUE)

#Attach PulledMain to Compiled Sheet
addWorksheet(CompiledSheet , "Old_Main")
writeData(CompiledSheet , sheet = "Old_Main", x = PulledMain)

#Pull out skus
PulledMain$sku <- as.character(PulledMain$sku)
MainSku<-PulledMain$sku
	
#Pull out any non-discontinued skus
UnDeletedSKU <- subset(PulledMain, delete=="N" & PulledMain$type=="simple", select=(sku))
UnDeletedSKU <- as.character(UnDeletedSKU$sku)

print("MainSheet Created")

#=========================================================== #(C1AE)Extract the Main sheet(C1AE)
#=========================================================== #(C1BS)Conduct Match with Formatted Jobber(C1BS)
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

print("Simple Match Completed")

#identical(nrow(UpdatedSkuListSimple),length(REF.NewSkuList)+ length(REF.DiscontSkuList))
#=========================================================== #(C1BE)Conduct Match with Formatted Jobber(C1BE)
############################################################ #(C1E)Extract Main sheet & Conduct Match and output ???? (C1E)
############################################################ #(D1S)Extract Invantory File(D1S)
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
PulledIF <- subset(PulledIF , select = c("Numb_Sku", "price","TotalQty", "CaseQty","Weight","Height","Length","Width","product_name", "upc"))

#Merge the final pulledIF with the simple match
Update.PostIF.Merge = merge(UpdatedSkuListSimple, PulledIF,  by="Numb_Sku", all = TRUE)

print("Invantory File Created")

############################################################ #(D1E)Extract Invantory File(D1E)
############################################################ #(E1S)Extract DCI-Partsapp/digitalAsset file(E1S)

#Create empty Data frame to collect the multiple DCI folder data
PooledPartsApp = data.frame()
PooledDigitalAsset = data.frame()

#*********************************************************** #(E2C1S)Fine the latest file within the specific DCI folder
	#Change the directory to temp folder incase any files are outputted
	setwd("//192.168.2.32/Group/Data Team/Abul/5. Temp Folder")
	
	#Create DCI Folder Path
	DCIPath = paste("//192.168.2.32/GoogleDrive/FTP_Downloads/DCI_Files/",as.character(DCIFolderList), sep = "", collapse = NULL)
	
	#Identify the Latest DCI file
	LatestDCIFile = sort(list.files(DCIPath , pattern = "*.zip"),decreasing = TRUE)[1]
	
	#Create Path for latest File
	LatestDCILocation = paste(as.character(DCIPath) ,as.character(LatestDCIFile), sep = "/", collapse = NULL)

#*********************************************************** #(E2C1E)Fine the latest file within the specific DCI folder
#*********************************************************** #(E2C2S)Find, upzip, remove duplicates and rbind to PooledPartsApp 
	#find the Parts App file & Load it
	PartsAppFile = paste(strsplit(LatestDCIFile , split='.zip', fixed=TRUE), "_Part_App.txt", sep = "",collapse = NULL)
	DCIPartapp= read.table(unzip(LatestDCILocation ,PartsAppFile ), sep ="|", header = TRUE, dec =".", quote = "" , stringsAsFactors=T, fill = TRUE)

	#Remove Duplicates
	DCIPartapp$Duplicate = paste(DCIPartapp$exppartno,":",DCIPartapp$expldescr, sep = "", collapse = NULL)
	DCIPartapp = DCIPartapp[!duplicated(DCIPartapp$Duplicate),]
	
	#Add # Infront of Sku Parts App
	DCIPartapp$Numb_Sku = paste("#",as.character(DCIPartapp$exppartno), sep = "", collapse = NULL)

	#Stick DCIPartapp into PooledPartsApp 
	PooledPartsApp = rbind(PooledPartsApp, DCIPartapp)

print("Parts App Processed")

#*********************************************************** #(E2C2E)Find, upzip, remove duplicates and rbind to PooledPartsApp 
#*********************************************************** #(E2C3S)Find, upzip and rbind to PooledDigitalAsset 
	#Find the Digital Asset file
	DigitalAssetFile = paste(strsplit(LatestDCIFile , split='.zip', fixed=TRUE), "_DigitalAsset.txt", sep = "",collapse = NULL)
	DCIDigitalAsset= read.table(unzip(LatestDCILocation ,DigitalAssetFile ), sep ="|", header = TRUE, dec =".", quote = "" , stringsAsFactors=T, fill = TRUE)

	#Add # Infront of Sku Digital Asset
	DCIDigitalAsset$Numb_Sku = paste("#",as.character(DCIDigitalAsset$exppartno), sep = "", collapse = NULL)

	#Stick DCIDigitalAsset into PooledDigitalAsset 
	PooledDigitalAsset = rbind(PooledDigitalAsset , DCIDigitalAsset)

print("Digital Asset Processed")

#*********************************************************** #(E2C3E)Find, upzip and rbind to PooledDigitalAsset 
#Removed duplicates from across Part Apps
PooledPartsApp$Duplicate = paste(PooledPartsApp$exppartno,":",PooledPartsApp$expldescr, sep = "", collapse = NULL)
PooledPartsApp = PooledPartsApp[!duplicated(PooledPartsApp$Duplicate),]

#Trim the PooledPartsApp for necessary information
PooledPartsApp <- subset(PooledPartsApp, select = c("Numb_Sku", "expldescr","fnstring", "merchname","dciptdescr"))
#-----------------------------------
#Merge the PooledPartsApp with Update.PostIF.Merge to create Update.PostPA.Merge
Update.PostConversion.Merge = merge(Update.PostIF.Merge, PooledPartsApp,  by=c("Numb_Sku"), all = TRUE)
Update.PostPA.Merge = merge(Update.PostConversion.Merge, REF.Conversion,  by=c("dciptdescr", "merchname"), all = TRUE)

############################################################ #(E1E)Extract DCI-Partsapp/digitalAsset file(E1E)
############################################################ #(F1S)Processing Images(E1S)
#If ImageDecisionAction is 1, then do the following, which is to process the images 
if(ImageDecisionAction ==1){
#############################

print("Processing Images Now")

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

print("Image Process Complete")

#############################
}

############################################################ #(F1E)Processing Images(E1E)
############################################################ #(G1S)Tidy up Update.PostPA.Merge for output
#Subset off revised skus
FinalUpdateFile = subset(Update.PostPA.Merge, RevisedSku=="Revised")

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

############################################################ #Output file Logic
############################################################ # Create All Unique Output folders
#Input, Backup and output folder names
InputFolder = "//192.168.2.32/Group/Data Team/Brand_Update_Location/1_Input_Folder"
BackUpFolder = "//192.168.2.32/Group/Data Team/Brand_Update_Location/1a_Input_Folder_BACKUP"
TEMPFolder = "//192.168.2.32/Group/Data Team/Brand_Update_Location/1b_Input_Folder_TEMP"
OutputFolder = "//192.168.2.32/Group/Data Team/Brand_Update_Location/2_Output_Folder"

#Check if FileOutput is 1
if(Final1_Test0== 1){
		
#Make output & Backup Folder
#----------------BackUP Folder------
#Parent BackUp Folder
Brand_BackUpLocation = paste(BackUpFolder,BrandFolderName, sep = "/", collapse = NULL)
	dir.create(paste(BackUpFolder,BrandFolderName, sep = "/", collapse = NULL)) 

#Child Backup Image Folder
Brand_BackUpIMAGELocation = paste(BackUpFolder,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL)
	dir.create(paste(BackUpFolder,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL)) 

#Child Backup Reference Folder
Brand_BackUpREFERENCELocation = paste(BackUpFolder,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL)
	dir.create(paste(BackUpFolder,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL)) 

#----------------Output Folder------
#Parent Output Folder
Brand_OutputLocation = paste(OutputFolder,BrandFolderName, sep = "/", collapse = NULL)
	dir.create(paste(OutputFolder,BrandFolderName , sep = "/", collapse = NULL))

#Child Output Image Folder
Brand_OutputIMAGELocation = paste(OutputFolder,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL)
	dir.create(paste(OutputFolder,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL))

#Child Output Reference Folder
Brand_OutputREFERENCELocation = paste(OutputFolder,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL)
	dir.create(paste(OutputFolder,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL))

print("Folders Created")

#----------Reference File Output---------
#Output the mainsheet into child reference folders
MSFileName = paste("MainSheet" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PulledMain, file = paste(Brand_BackUpREFERENCELocation, MSFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)
write.csv(PulledMain, file = paste(Brand_OutputREFERENCELocation, MSFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

#Output the PulledIF  into child reference folders
PulledIF_Name = paste("InventoryFile" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PulledIF , file = paste(Brand_BackUpREFERENCELocation, PulledIF_Name, sep = "/", collapse = NULL) , na="", row.names=FALSE)
write.csv(PulledIF , file = paste(Brand_OutputREFERENCELocation, PulledIF_Name , sep = "/", collapse = NULL) , na="", row.names=FALSE)

#Output the PooledPartsApp into child reference folders
PAFileName = paste("PartsApp " ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PooledPartsApp , file = paste(Brand_BackUpREFERENCELocation, PAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)
write.csv(PooledPartsApp , file = paste(Brand_OutputREFERENCELocation, PAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

#Output the PooledDigitalAsset into child reference folders
DAFileName = paste("DigitalAsset " ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PooledDigitalAsset , file = paste(Brand_BackUpREFERENCELocation, DAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)
write.csv(PooledDigitalAsset , file = paste(Brand_OutputREFERENCELocation, DAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

UDFileName = paste("UpdateFile" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(CompleteUpdate , file = paste(Brand_BackUpREFERENCELocation, UDFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)
write.csv(CompleteUpdate , file = paste(Brand_OutputREFERENCELocation, UDFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)


#-----------Create CompiledSheet------------
#Attach Updatefile to CompiledSheet
addWorksheet(CompiledSheet , "Formated_Jobber")
writeData(CompiledSheet, sheet = "Formated_Jobber", x = UpdateFile)

#Attach CompleteUpdate to CompiledSheet
addWorksheet(CompiledSheet , "Update_Analysis")
writeData(CompiledSheet , sheet = "Update_Analysis", x = CompleteUpdate)

#Attach Upfate.PostIF.Merge with CompiledSheet
addWorksheet(CompiledSheet , "Inventory_File")
writeData(CompiledSheet , sheet = "Inventory_File", x = PulledIF)

#Attach PooledPartsApp to CompiledSheet
addWorksheet(CompiledSheet , "Parts_App")
writeData(CompiledSheet , sheet = "Parts_App", x = PooledPartsApp)

#Attach DAFileName to CompiledSheet
addWorksheet(CompiledSheet , "Digital_Asset")
writeData(CompiledSheet , sheet = "Digital_Asset", x = PooledDigitalAsset)

saveWorkbook(CompiledSheet, paste(Brand_OutputLocation, UDFileName , sep = "/", collapse = NULL), overwrite = FALSE)
saveWorkbook(CompiledSheet, paste(Brand_BackUpLocation, UDFileName , sep = "/", collapse = NULL), overwrite = FALSE)


} else{

#----------------TEMP Folder------
#Parent Output Folder
Brand_TEMPLocation = paste(TEMPFolder ,BrandFolderName, sep = "/", collapse = NULL)
	dir.create(paste(TEMPFolder ,BrandFolderName , sep = "/", collapse = NULL))

#Child Output Image Folder
Brand_TEMPIMAGELocation = paste(TEMPFolder ,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL)
	dir.create(paste(TEMPFolder ,BrandFolderName,"Image_Folder", sep = "/", collapse = NULL))

#Child Output Reference Folder
Brand_TempREFERENCELocation = paste(TEMPFolder ,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL)
	dir.create(paste(TEMPFolder ,BrandFolderName,"Reference_Folder", sep = "/", collapse = NULL))

print("Folders Created")

#----------Reference File Output---------
#Output the mainsheet into child reference folders
MSFileName = paste("MainSheet" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PulledMain, file = paste(Brand_TempREFERENCELocation, MSFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

#Output the PulledIF  into child reference folders
PulledIF_Name = paste("InventoryFile" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PulledIF , file = paste(Brand_TempREFERENCELocation, PulledIF_Name, sep = "/", collapse = NULL) , na="", row.names=FALSE)

#Output the PooledPartsApp into child reference folders
PAFileName = paste("PartsApp " ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PooledPartsApp , file = paste(Brand_TempREFERENCELocation, PAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

#Output the PooledDigitalAsset into child reference folders
DAFileName = paste("DigitalAsset " ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(PooledDigitalAsset , file = paste(Brand_TempREFERENCELocation, DAFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

UDFileName = paste("UpdateFile" ,BrandFolderName, "csv", sep = ".", collapse = NULL)
write.csv(CompleteUpdate , file = paste(Brand_TempREFERENCELocation, UDFileName , sep = "/", collapse = NULL) , na="", row.names=FALSE)

#-----------Create CompiledSheet------------
#Attach Updatefile to CompiledSheet
addWorksheet(CompiledSheet , "Formated_Jobber")
writeData(CompiledSheet, sheet = "Formated_Jobber", x = UpdateFile)

#Attach CompleteUpdate to CompiledSheet
addWorksheet(CompiledSheet , "Update_Analysis")
writeData(CompiledSheet , sheet = "Update_Analysis", x = CompleteUpdate)

#Attach Upfate.PostIF.Merge with CompiledSheet
addWorksheet(CompiledSheet , "Inventory_File")
writeData(CompiledSheet , sheet = "Inventory_File", x = PulledIF)

#Attach PooledPartsApp to CompiledSheet
addWorksheet(CompiledSheet , "Parts_App")
writeData(CompiledSheet , sheet = "Parts_App", x = PooledPartsApp)

#Attach DAFileName to CompiledSheet
addWorksheet(CompiledSheet , "Digital_Asset")
writeData(CompiledSheet , sheet = "Digital_Asset", x = PooledDigitalAsset)

saveWorkbook(CompiledSheet, paste(Brand_TEMPLocation, UDFileName , sep = "/", collapse = NULL), overwrite = FALSE)
} 

############################################################ #(G1E)Tidy up Update.PostPA.Merge for output
print("ALL DONE!")

}
############################################################ #(A1E)Create Update.Brand Function (A1E)

