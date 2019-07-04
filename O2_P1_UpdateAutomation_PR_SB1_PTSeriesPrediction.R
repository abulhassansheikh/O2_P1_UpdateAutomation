###############################################
###############################################
###Prediction Logic for PT and Series depending on Avaliable Sources
#Jobber + DCI + IF
if(length(string_IF) == 2 & length(string_DCI) == 2){

	#Make Prediction df via merging
	PredictPT_jobber = merge(MS_PTlabel, string_jobber, by = "Numb_Sku", all = TRUE)
	PredictPT_IF = merge(MS_PTlabel, string_IF, by = "Numb_Sku", all = TRUE)
	PredictPT_DCI = merge(MS_PTlabel, string_DCI, by = "Numb_Sku", all = TRUE)
	PredictSE_jobber = merge(MS_Serieslabel, string_jobber, by = "Numb_Sku", all = TRUE)
	PredictSE_IF = merge(MS_Serieslabel, string_IF, by = "Numb_Sku", all = TRUE)
	PredictSE_DCI = merge(MS_Serieslabel, string_DCI, by = "Numb_Sku", all = TRUE)

	#Make Prediction from prediction df
	PrePTResult_jobber = NBPredict(PredictionDF = PredictPT_jobber, source = "jobber")
	PrePTResult_IF = NBPredict(PredictionDF = PredictPT_IF, source = "IF")
	PrePTResult_DCI = NBPredict(PredictionDF = PredictPT_DCI, source = "DCI")
	PreSEResult_jobber = NBPredict(PredictionDF = PredictSE_jobber, source = "jobber")
	PreSEResult_IF = NBPredict(PredictionDF = PredictSE_IF, source = "IF")
	PreSEResult_DCI = NBPredict(PredictionDF = PredictSE_DCI, source = "DCI")

	#Subset result df
	PrePTResult_jobber = subset(PrePTResult_jobber, select=c("Numb_Sku", "PredictedLabel_jobber", "EasyConfidence_jobber", "Difference_jobber" ))
	PrePTResult_IF = subset(PrePTResult_IF , select=c("Numb_Sku", "PredictedLabel_IF", "EasyConfidence_IF", "Difference_IF" ))
	PrePTResult_DCI = subset(PrePTResult_DCI, select=c("Numb_Sku", "PredictedLabel_DCI", "EasyConfidence_DCI", "Difference_DCI" ))
	PreSEResult_jobber = subset(PreSEResult_jobber, select=c("Numb_Sku", "PredictedLabel_jobber", "EasyConfidence_jobber", "Difference_jobber" ))
	PreSEResult_IF = subset(PreSEResult_IF , select=c("Numb_Sku", "PredictedLabel_IF", "EasyConfidence_IF", "Difference_IF" ))
	PreSEResult_DCI = subset(PreSEResult_DCI, select=c("Numb_Sku", "PredictedLabel_DCI", "EasyConfidence_DCI", "Difference_DCI" ))

	#Merge to aquire FinalPTPredict df
	FinalPTPredict = merge(PrePTResult_jobber, PrePTResult_IF, by = "Numb_Sku", all = TRUE)
	FinalPTPredict = merge(FinalPTPredict, PrePTResult_DCI, by = "Numb_Sku", all = TRUE)
	FinalSEPredict = merge(PreSEResult_jobber, PreSEResult_IF, by = "Numb_Sku", all = TRUE)
	FinalSEPredict = merge(FinalSEPredict, PreSEResult_DCI, by = "Numb_Sku", all = TRUE)

	#Decide the part type per sku
	##When do all three prediction match?
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_jobber) == as.character(FinalPTPredict$PredictedLabel_IF) &
					   as.character(FinalPTPredict$PredictedLabel_jobber) == as.character(FinalPTPredict$PredictedLabel_DCI) &
					  as.character(FinalPTPredict$PredictedLabel_IF) == as.character(FinalPTPredict$PredictedLabel_DCI)] <- "All_Match" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_jobber) == as.character(FinalSEPredict$PredictedLabel_IF) &
					   as.character(FinalSEPredict$PredictedLabel_jobber) == as.character(FinalSEPredict$PredictedLabel_DCI) &
					  as.character(FinalSEPredict$PredictedLabel_IF) == as.character(FinalSEPredict$PredictedLabel_DCI)] <- "All_Match" 

	##When do all three prediction NOT match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_jobber) != as.character(FinalPTPredict$PredictedLabel_IF) &
					   as.character(FinalPTPredict$PredictedLabel_jobber) != as.character(FinalPTPredict$PredictedLabel_DCI) &
					  as.character(FinalPTPredict$PredictedLabel_IF) != as.character(FinalPTPredict$PredictedLabel_DCI)] <- "No_Match" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_jobber) != as.character(FinalSEPredict$PredictedLabel_IF) &
					   as.character(FinalSEPredict$PredictedLabel_jobber) != as.character(FinalSEPredict$PredictedLabel_DCI) &
					  as.character(FinalSEPredict$PredictedLabel_IF) != as.character(FinalSEPredict$PredictedLabel_DCI)] <- "No_Match" 

	##Determin if Jobber and DCI Missing
	FinalPTPredict$PTMatch[is.na(FinalPTPredict$PredictedLabel_jobber) & is.na(FinalPTPredict$PredictedLabel_DCI)] <- "IF_Only" 
	FinalSEPredict$SEMatch[is.na(FinalSEPredict$PredictedLabel_jobber) & is.na(FinalSEPredict$PredictedLabel_DCI)] <- "IF_Only" 
	##Determin if Jobber and IF Missing
	FinalPTPredict$PTMatch[is.na(FinalPTPredict$PredictedLabel_jobber) & is.na(FinalPTPredict$PredictedLabel_IF)] <- "DCI_Only" 
	FinalSEPredict$SEMatch[is.na(FinalSEPredict$PredictedLabel_jobber) & is.na(FinalSEPredict$PredictedLabel_IF)] <- "DCI_Only" 
	##Determin if DCI and IF Missing
	FinalPTPredict$PTMatch[is.na(FinalPTPredict$PredictedLabel_DCI) & is.na(FinalPTPredict$PredictedLabel_IF)] <- "Jobber_Only"
	FinalSEPredict$SEMatch[is.na(FinalSEPredict$PredictedLabel_DCI) & is.na(FinalSEPredict$PredictedLabel_IF)] <- "Jobber_Only"

	##Jobber and DCI match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_jobber) == as.character(FinalPTPredict$PredictedLabel_DCI) & is.na(FinalPTPredict$PTMatch)] <- "Jobber_DCI_Match" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_jobber) == as.character(FinalSEPredict$PredictedLabel_DCI) & is.na(FinalSEPredict$SEMatch)] <- "Jobber_DCI_Match" 
	##Jobber and IF match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_jobber) == as.character(FinalPTPredict$PredictedLabel_IF) & is.na(FinalPTPredict$PTMatch)] <- "Jobber_IF_Match" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_jobber) == as.character(FinalSEPredict$PredictedLabel_IF) & is.na(FinalSEPredict$SEMatch)] <- "Jobber_IF_Match" 
	##DCI and IF match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_DCI) == as.character(FinalPTPredict$PredictedLabel_IF) & is.na(FinalPTPredict$PTMatch)] <- "DCI_IF_Match" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_DCI) == as.character(FinalSEPredict$PredictedLabel_IF) & is.na(FinalSEPredict$SEMatch)] <- "DCI_IF_Match" 

	##Jobber and DCI match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_jobber) != as.character(FinalPTPredict$PredictedLabel_DCI) & is.na(FinalPTPredict$PTMatch)] <- "Jobber_DCI_Present" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_jobber) != as.character(FinalSEPredict$PredictedLabel_DCI) & is.na(FinalSEPredict$SEMatch)] <- "Jobber_DCI_Present" 
	##Jobber and IF match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_jobber) != as.character(FinalPTPredict$PredictedLabel_IF) & is.na(FinalPTPredict$PTMatch)] <- "Jobber_IF_Present" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_jobber) != as.character(FinalSEPredict$PredictedLabel_IF) & is.na(FinalSEPredict$SEMatch)] <- "Jobber_IF_Present" 
	##DCI and IF match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_DCI) != as.character(FinalPTPredict$PredictedLabel_IF) & is.na(FinalPTPredict$PTMatch)] <- "DCI_IF_Present" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_DCI) != as.character(FinalSEPredict$PredictedLabel_IF) & is.na(FinalSEPredict$SEMatch)] <- "DCI_IF_Present" 

	#Analyze match result
	FinalPTPredict$PT = "Blank"
	FinalPTPredict$PTConfidence= "Blank"
	FinalSEPredict$SE = "Blank"
	FinalSEPredict$SEConfidence= "Blank"

	###Process FinalPTPredict
	for(i in 1:nrow(FinalPTPredict)){

		MatchValue = FinalPTPredict$PTMatch[i]
		PT_jobber = FinalPTPredict$PredictedLabel_jobber[i]
		PT_IF = FinalPTPredict$PredictedLabel_IF[i]
		PT_DCI = FinalPTPredict$PredictedLabel_DCI[i]
		DiffJob = as.numeric(FinalPTPredict$Difference_jobber[i])
		DiffIF = as.numeric(FinalPTPredict$Difference_IF[i])
		DiffDCI = as.numeric(FinalPTPredict$Difference_DCI[i])
		ConJob = FinalPTPredict$EasyConfidence_jobber[i]
		ConIF = FinalPTPredict$EasyConfidence_IF[i]
		ConDCI = FinalPTPredict$EasyConfidence_DCI[i]

		if(MatchValue == "All_Match"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- "High"
		} else if(MatchValue == "No_Match"){
	
				if(DiffJob > DiffIF & DiffJob > DiffDCI){
					FinalPTPredict$PT[i] <- PT_jobber
					FinalPTPredict$PTConfidence[i] <- ConJob
				} else if(DiffIF > DiffJob & DiffIF > DiffDCI){
					FinalPTPredict$PT[i] <- PT__IF
					FinalPTPredict$PTConfidence[i] <- ConIF
				} else if(DiffDCI > DiffJob & DiffDCI> DiffIF ){
					FinalPTPredict$PT[i] <- PT_DCI
					FinalPTPredict$PTConfidence[i] <- ConDCI
				} else{}
		
		} else if(MatchValue == "IF_Only"){
			FinalPTPredict$PT[i] <- PT_IF
			FinalPTPredict$PTConfidence[i] <- ConIF

		} else if(MatchValue == "DCI_Only"){
			FinalPTPredict$PT[i] <- PT_DCI
			FinalPTPredict$PTConfidence[i] <- ConDCI
		
		} else if(MatchValue == "Jobber_Only"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- ConJob
		
		} else if(MatchValue == "Jobber_DCI_Match"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- "Medium"
		
		} else if(MatchValue == "Jobber_IF_Match"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- "Medium"
		
		} else if(MatchValue == "DCI_IF_Match"){
			FinalPTPredict$PT[i] <- PT_IF
			FinalPTPredict$PTConfidence[i] <- "Medium"
		
		} else if(MatchValue == "Jobber_DCI_Present"){
			if(DiffJob>DiffDCI){
				FinalPTPredict$PT[i] <- PT_jobber
				FinalPTPredict$PTConfidence[i] <- ConJob
			} else{
				FinalPTPredict$PT[i] <- PT_DCI
				FinalPTPredict$PTConfidence[i] <- ConDCI
			}
		
		} else if(MatchValue == "Jobber_IF_Present"){
			if(DiffJob>DiffIF){
				FinalPTPredict$PT[i] <- PT_jobber
				FinalPTPredict$PTConfidence[i] <- ConJob
			} else{
				FinalPTPredict$PT[i] <- PT_IF
				FinalPTPredict$PTConfidence[i] <- ConIF
			}
		
		} else if(MatchValue == "DCI_IF_Present"){
			if(DiffDCI>DiffIF){
				FinalPTPredict$PT[i] <- PT_DCI
				FinalPTPredict$PTConfidence[i] <- ConDCI
			} else{
				FinalPTPredict$PT[i] <- PT_IF
				FinalPTPredict$PTConfidence[i] <- ConIF
			}
		
		} else{}
	}	

	###Process FinalSEPredict
	for(i in 1:nrow(FinalSEPredict)){
		MatchValue = FinalSEPredict$SEMatch[i]
		SE_jobber = FinalSEPredict$PredictedLabel_jobber[i]
		SE_IF = FinalSEPredict$PredictedLabel_IF[i]
		SE_DCI = FinalSEPredict$PredictedLabel_DCI[i]
		DiffJob = as.numeric(FinalSEPredict$Difference_jobber[i])
		DiffIF = as.numeric(FinalSEPredict$Difference_IF[i])
		DiffDCI = as.numeric(FinalSEPredict$Difference_DCI[i])
		ConJob = FinalSEPredict$EasyConfidence_jobber[i]
		ConIF = FinalSEPredict$EasyConfidence_IF[i]
		ConDCI = FinalSEPredict$EasyConfidence_DCI[i]

		if(MatchValue == "All_Match"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- "High"
		} else if(MatchValue == "No_Match"){
	
				if(DiffJob > DiffIF & DiffJob > DiffDCI){
					FinalSEPredict$SE[i] <- SE_jobber
					FinalSEPredict$SEConfidence[i] <- ConJob
				} else if(DiffIF > DiffJob & DiffIF > DiffDCI){
					FinalSEPredict$SE[i] <- SE_IF
					FinalSEPredict$SEConfidence[i] <- ConIF
				} else if(DiffDCI > DiffJob & DiffDCI> ConIF ){
					FinalSEPredict$SE[i] <- SE_DCI
					FinalSEPredict$SEConfidence[i] <- ConDCI
				} else{}

		} else if(MatchValue == "IF_Only"){
			FinalSEPredict$SE[i] <- SE_IF
			FinalSEPredict$SEConfidence[i] <- ConIF

		} else if(MatchValue == "DCI_Only"){
			FinalSEPredict$SE[i] <- SE_DCI
			FinalSEPredict$SEConfidence[i] <- ConDCI
		
		} else if(MatchValue == "Jobber_Only"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- ConJob
		
		} else if(MatchValue == "Jobber_DCI_Match"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- "Medium"
		
		} else if(MatchValue == "Jobber_IF_Match"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- "Medium"
		
		} else if(MatchValue == "DCI_IF_Match"){
			FinalSEPredict$SE[i] <- SE_IF
			FinalSEPredict$SEConfidence[i] <- "Medium"
		
		} else if(MatchValue == "Jobber_DCI_Present"){
			if(DiffJob>DiffDCI){
				FinalSEPredict$SE[i] <- SE_jobber
				FinalSEPredict$SEConfidence[i] <- ConJob
			} else{
				FinalSEPredict$SE[i] <- SE_DCI
				FinalSEPredict$SEConfidence[i] <- ConDCI
			}
		
		} else if(MatchValue == "Jobber_IF_Present"){
			if(DiffJob>DiffIF){
				FinalSEPredict$SE[i] <- SE_jobber
				FinalSEPredict$SEConfidence[i] <- ConJob
			} else{
				FinalSEPredict$SE[i] <- SE_IF
				FinalSEPredict$SEConfidence[i] <- ConIF
			}
		
		} else if(MatchValue == "DCI_IF_Present"){
			if(DiffDCI>DiffIF){
				FinalSEPredict$SE[i] <- SE_DCI
				FinalSEPredict$SEConfidence[i] <- ConDCI
			} else{
				FinalSEPredict$SE[i] <- SE_IF
				FinalSEPredict$SEConfidence[i] <- ConIF
			}
		
		} else{}
	}

#Jobber + DCI
} else if(length(string_IF) == 1 & length(string_DCI) == 2){

	#Make Prediction df via merging
	PredictPT_jobber = merge(MS_PTlabel, string_jobber, by = "Numb_Sku", all = TRUE)
	PredictPT_DCI = merge(MS_PTlabel, string_DCI, by = "Numb_Sku", all = TRUE)
	PredictSE_jobber = merge(MS_Serieslabel, string_jobber, by = "Numb_Sku", all = TRUE)
	PredictSE_DCI = merge(MS_Serieslabel, string_DCI, by = "Numb_Sku", all = TRUE)

	#Make Prediction from prediction df
	PrePTResult_jobber = NBPredict(PredictionDF = PredictPT_jobber, source = "jobber")
	PrePTResult_DCI = NBPredict(PredictionDF = PredictPT_DCI, source = "DCI")
	PreSEResult_jobber = NBPredict(PredictionDF = PredictSE_jobber, source = "jobber")
	PreSEResult_DCI = NBPredict(PredictionDF = PredictSE_DCI, source = "DCI")

	#Subset result df
	PrePTResult_jobber = subset(PrePTResult_jobber, select=c("Numb_Sku", "PredictedLabel_jobber", "EasyConfidence_jobber", "Difference_jobber" ))
	PrePTResult_DCI = subset(PrePTResult_DCI, select=c("Numb_Sku", "PredictedLabel_DCI", "EasyConfidence_DCI", "Difference_DCI" ))
	PreSEResult_jobber = subset(PreSEResult_jobber, select=c("Numb_Sku", "PredictedLabel_jobber", "EasyConfidence_jobber", "Difference_jobber" ))
	PreSEResult_DCI = subset(PreSEResult_DCI, select=c("Numb_Sku", "PredictedLabel_DCI", "EasyConfidence_DCI", "Difference_DCI" ))

	#Merge to aquire FinalPTPredict df
	FinalPTPredict = merge(PrePTResult_jobber, PrePTResult_DCI, by = "Numb_Sku", all = TRUE)
	FinalSEPredict = merge(PreSEResult_jobber, PreSEResult_DCI, by = "Numb_Sku", all = TRUE)

	##Jobber and DCI match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_jobber) == as.character(FinalPTPredict$PredictedLabel_DCI)] <- "Jobber_DCI_Match" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_jobber) == as.character(FinalSEPredict$PredictedLabel_DCI)] <- "Jobber_DCI_Match" 

	##Jobber and DCI dont match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_jobber) != as.character(FinalPTPredict$PredictedLabel_DCI) & is.na(FinalPTPredict$PTMatch)] <- "Jobber_DCI_Present" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_jobber) != as.character(FinalSEPredict$PredictedLabel_DCI) & is.na(FinalSEPredict$SEMatch)] <- "Jobber_DCI_Present"

	##Determin if DCI Missing
	FinalPTPredict$PTMatch[is.na(FinalPTPredict$PredictedLabel_DCI)] <- "Jobber_Only"
	FinalSEPredict$SEMatch[is.na(FinalSEPredict$PredictedLabel_DCI)] <- "Jobber_Only"

	##Determin if Jobber Missing
	FinalPTPredict$PTMatch[is.na(FinalPTPredict$PredictedLabel_jobber)] <- "DCI_Only" 
	FinalSEPredict$SEMatch[is.na(FinalSEPredict$PredictedLabel_jobber)] <- "DCI_Only" 


	#Analyze match result
	FinalPTPredict$PT = "Blank"
	FinalPTPredict$PTConfidence= "Blank"
	FinalSEPredict$SE = "Blank"
	FinalSEPredict$SEConfidence= "Blank"

	###Process FinalPTPredict
	for(i in 1:nrow(FinalPTPredict)){

		MatchValue = FinalPTPredict$PTMatch[i]
		PT_jobber = FinalPTPredict$PredictedLabel_jobber[i]
		PT_IF = FinalPTPredict$PredictedLabel_IF[i]
		PT_DCI = FinalPTPredict$PredictedLabel_DCI[i]
		DiffJob = as.numeric(FinalPTPredict$Difference_jobber[i])
		DiffIF = as.numeric(FinalPTPredict$Difference_IF[i])
		DiffDCI = as.numeric(FinalPTPredict$Difference_DCI[i])
		ConJob = FinalPTPredict$EasyConfidence_jobber[i]
		ConIF = FinalPTPredict$EasyConfidence_IF[i]
		ConDCI = FinalPTPredict$EasyConfidence_DCI[i]

		if(MatchValue == "All_Match"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- "High"
		} else if(MatchValue == "No_Match"){
	
				if(DiffJob > DiffIF & DiffJob > DiffDCI){
					FinalPTPredict$PT[i] <- PT_jobber
					FinalPTPredict$PTConfidence[i] <- ConJob
				} else if(DiffIF > DiffJob & DiffIF > DiffDCI){
					FinalPTPredict$PT[i] <- PT__IF
					FinalPTPredict$PTConfidence[i] <- ConIF
				} else if(DiffDCI > DiffJob & DiffDCI> DiffIF ){
					FinalPTPredict$PT[i] <- PT_DCI
					FinalPTPredict$PTConfidence[i] <- ConDCI
				} else{}
		
		} else if(MatchValue == "IF_Only"){
			FinalPTPredict$PT[i] <- PT_IF
			FinalPTPredict$PTConfidence[i] <- ConIF

		} else if(MatchValue == "DCI_Only"){
			FinalPTPredict$PT[i] <- PT_DCI
			FinalPTPredict$PTConfidence[i] <- ConDCI
		
		} else if(MatchValue == "Jobber_Only"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- ConJob
		
		} else if(MatchValue == "Jobber_DCI_Match"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "Jobber_IF_Match"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "DCI_IF_Match"){
			FinalPTPredict$PT[i] <- PT_IF
			FinalPTPredict$PTConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "Jobber_DCI_Present"){
			if(DiffJob>DiffDCI){
				FinalPTPredict$PT[i] <- PT_jobber
				FinalPTPredict$PTConfidence[i] <- ConJob
			} else{
				FinalPTPredict$PT[i] <- PT_DCI
				FinalPTPredict$PTConfidence[i] <- ConDCI
			}
		
		} else if(MatchValue == "Jobber_IF_Present"){
			if(DiffJob>DiffIF){
				FinalPTPredict$PT[i] <- PT_jobber
				FinalPTPredict$PTConfidence[i] <- ConJob
			} else{
				FinalPTPredict$PT[i] <- PT_IF
				FinalPTPredict$PTConfidence[i] <- ConIF
			}
		
		} else if(MatchValue == "DCI_IF_Present"){
			if(DiffDCI>DiffIF){
				FinalPTPredict$PT[i] <- PT_DCI
				FinalPTPredict$PTConfidence[i] <- ConDCI
			} else{
				FinalPTPredict$PT[i] <- PT_IF
				FinalPTPredict$PTConfidence[i] <- ConIF
			}
		
		} else{}
	}	

	###Process FinalSEPredict
	for(i in 1:nrow(FinalSEPredict)){
		MatchValue = FinalSEPredict$SEMatch[i]
		SE_jobber = FinalSEPredict$PredictedLabel_jobber[i]
		SE_IF = FinalSEPredict$PredictedLabel_IF[i]
		SE_DCI = FinalSEPredict$PredictedLabel_DCI[i]
		DiffJob = as.numeric(FinalSEPredict$Difference_jobber[i])
		DiffIF = as.numeric(FinalSEPredict$Difference_IF[i])
		DiffDCI = as.numeric(FinalSEPredict$Difference_DCI[i])
		ConJob = FinalSEPredict$EasyConfidence_jobber[i]
		ConIF = FinalSEPredict$EasyConfidence_IF[i]
		ConDCI = FinalSEPredict$EasyConfidence_DCI[i]

		if(MatchValue == "All_Match"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- "High"
		} else if(MatchValue == "No_Match"){
	
				if(DiffJob > DiffIF & DiffJob > DiffDCI){
					FinalSEPredict$SE[i] <- SE_jobber
					FinalSEPredict$SEConfidence[i] <- ConJob
				} else if(DiffIF > DiffJob & DiffIF > DiffDCI){
					FinalSEPredict$SE[i] <- SE_IF
					FinalSEPredict$SEConfidence[i] <- ConIF
				} else if(DiffDCI > DiffJob & DiffDCI> ConIF ){
					FinalSEPredict$SE[i] <- SE_DCI
					FinalSEPredict$SEConfidence[i] <- ConDCI
				} else{}

		} else if(MatchValue == "IF_Only"){
			FinalSEPredict$SE[i] <- SE_IF
			FinalSEPredict$SEConfidence[i] <- ConIF

		} else if(MatchValue == "DCI_Only"){
			FinalSEPredict$SE[i] <- SE_DCI
			FinalSEPredict$SEConfidence[i] <- ConDCI
		
		} else if(MatchValue == "Jobber_Only"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- ConJob
		
		} else if(MatchValue == "Jobber_DCI_Match"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "Jobber_IF_Match"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "DCI_IF_Match"){
			FinalSEPredict$SE[i] <- SE_IF
			FinalSEPredict$SEConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "Jobber_DCI_Present"){
			if(DiffJob>DiffDCI){
				FinalSEPredict$SE[i] <- SE_jobber
				FinalSEPredict$SEConfidence[i] <- ConJob
			} else{
				FinalSEPredict$SE[i] <- SE_DCI
				FinalSEPredict$SEConfidence[i] <- ConDCI
			}
		
		} else if(MatchValue == "Jobber_IF_Present"){
			if(DiffJob>DiffIF){
				FinalSEPredict$SE[i] <- SE_jobber
				FinalSEPredict$SEConfidence[i] <- ConJob
			} else{
				FinalSEPredict$SE[i] <- SE_IF
				FinalSEPredict$SEConfidence[i] <- ConIF
			}
		
		} else if(MatchValue == "DCI_IF_Present"){
			if(DiffDCI>DiffIF){
				FinalSEPredict$SE[i] <- SE_DCI
				FinalSEPredict$SEConfidence[i] <- ConDCI
			} else{
				FinalSEPredict$SE[i] <- SE_IF
				FinalSEPredict$SEConfidence[i] <- ConIF
			}
		
		} else{}
	}


#Jobber + IF
} else if(length(string_IF) == 2 & length(string_DCI) == 1){

	#Make Prediction df via merging
	PredictPT_jobber = merge(MS_PTlabel, string_jobber, by = "Numb_Sku", all = TRUE)
	PredictPT_IF = merge(MS_PTlabel, string_IF, by = "Numb_Sku", all = TRUE)
	PredictSE_jobber = merge(MS_Serieslabel, string_jobber, by = "Numb_Sku", all = TRUE)
	PredictSE_IF = merge(MS_Serieslabel, string_IF, by = "Numb_Sku", all = TRUE)

	#Make Prediction from prediction df
	PrePTResult_jobber = NBPredict(PredictionDF = PredictPT_jobber, source = "jobber")
	PrePTResult_IF = NBPredict(PredictionDF = PredictPT_IF, source = "IF")
	PreSEResult_jobber = NBPredict(PredictionDF = PredictSE_jobber, source = "jobber")
	PreSEResult_IF = NBPredict(PredictionDF = PredictSE_IF, source = "IF")

	#Subset result df
	PrePTResult_jobber = subset(PrePTResult_jobber, select=c("Numb_Sku", "PredictedLabel_jobber", "EasyConfidence_jobber", "Difference_jobber" ))
	PrePTResult_IF = subset(PrePTResult_IF , select=c("Numb_Sku", "PredictedLabel_IF", "EasyConfidence_IF", "Difference_IF" ))
	PreSEResult_jobber = subset(PreSEResult_jobber, select=c("Numb_Sku", "PredictedLabel_jobber", "EasyConfidence_jobber", "Difference_jobber" ))
	PreSEResult_IF = subset(PreSEResult_IF , select=c("Numb_Sku", "PredictedLabel_IF", "EasyConfidence_IF", "Difference_IF" ))

	#Merge to aquire FinalPTPredict df
	FinalPTPredict = merge(PrePTResult_jobber, PrePTResult_IF, by = "Numb_Sku", all = TRUE)
	FinalSEPredict = merge(PreSEResult_jobber, PreSEResult_IF, by = "Numb_Sku", all = TRUE)

	##Jobber and IF match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_jobber) == as.character(FinalPTPredict$PredictedLabel_IF)] <- "Jobber_IF_Match" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_jobber) == as.character(FinalSEPredict$PredictedLabel_IF)] <- "Jobber_IF_Match" 
	
	##Jobber and IF Dont match
	FinalPTPredict$PTMatch[as.character(FinalPTPredict$PredictedLabel_jobber) != as.character(FinalPTPredict$PredictedLabel_IF) & is.na(FinalPTPredict$PTMatch)] <- "Jobber_IF_Present" 
	FinalSEPredict$SEMatch[as.character(FinalSEPredict$PredictedLabel_jobber) != as.character(FinalSEPredict$PredictedLabel_IF) & is.na(FinalSEPredict$SEMatch)] <- "Jobber_IF_Present" 

	##Determin if IF Missing
	FinalPTPredict$PTMatch[is.na(FinalPTPredict$PredictedLabel_IF)] <- "Jobber_Only"
	FinalSEPredict$SEMatch[is.na(FinalSEPredict$PredictedLabel_IF)] <- "Jobber_Only"

	##Determin if Jobber Missing
	FinalPTPredict$PTMatch[is.na(FinalPTPredict$PredictedLabel_jobber)] <- "IF_Only" 
	FinalSEPredict$SEMatch[is.na(FinalSEPredict$PredictedLabel_jobber)] <- "IF_Only"


	#Analyze match result
	FinalPTPredict$PT = "Blank"
	FinalPTPredict$PTConfidence= "Blank"
	FinalSEPredict$SE = "Blank"
	FinalSEPredict$SEConfidence= "Blank"

	###Process FinalPTPredict
	for(i in 1:nrow(FinalPTPredict)){

		MatchValue = FinalPTPredict$PTMatch[i]
		PT_jobber = FinalPTPredict$PredictedLabel_jobber[i]
		PT_IF = FinalPTPredict$PredictedLabel_IF[i]
		PT_DCI = FinalPTPredict$PredictedLabel_DCI[i]
		DiffJob = as.numeric(FinalPTPredict$Difference_jobber[i])
		DiffIF = as.numeric(FinalPTPredict$Difference_IF[i])
		DiffDCI = as.numeric(FinalPTPredict$Difference_DCI[i])
		ConJob = FinalPTPredict$EasyConfidence_jobber[i]
		ConIF = FinalPTPredict$EasyConfidence_IF[i]
		ConDCI = FinalPTPredict$EasyConfidence_DCI[i]

		if(MatchValue == "All_Match"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- "High"
		} else if(MatchValue == "No_Match"){
	
				if(DiffJob > DiffIF & DiffJob > DiffDCI){
					FinalPTPredict$PT[i] <- PT_jobber
					FinalPTPredict$PTConfidence[i] <- ConJob
				} else if(DiffIF > DiffJob & DiffIF > DiffDCI){
					FinalPTPredict$PT[i] <- PT__IF
					FinalPTPredict$PTConfidence[i] <- ConIF
				} else if(DiffDCI > DiffJob & DiffDCI> DiffIF ){
					FinalPTPredict$PT[i] <- PT_DCI
					FinalPTPredict$PTConfidence[i] <- ConDCI
				} else{}
		
		} else if(MatchValue == "IF_Only"){
			FinalPTPredict$PT[i] <- PT_IF
			FinalPTPredict$PTConfidence[i] <- ConIF

		} else if(MatchValue == "DCI_Only"){
			FinalPTPredict$PT[i] <- PT_DCI
			FinalPTPredict$PTConfidence[i] <- ConDCI
		
		} else if(MatchValue == "Jobber_Only"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- ConJob
		
		} else if(MatchValue == "Jobber_DCI_Match"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "Jobber_IF_Match"){
			FinalPTPredict$PT[i] <- PT_jobber
			FinalPTPredict$PTConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "DCI_IF_Match"){
			FinalPTPredict$PT[i] <- PT_IF
			FinalPTPredict$PTConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "Jobber_DCI_Present"){
			if(DiffJob>DiffDCI){
				FinalPTPredict$PT[i] <- PT_jobber
				FinalPTPredict$PTConfidence[i] <- ConJob
			} else{
				FinalPTPredict$PT[i] <- PT_DCI
				FinalPTPredict$PTConfidence[i] <- ConDCI
			}
		
		} else if(MatchValue == "Jobber_IF_Present"){
			if(DiffJob>DiffIF){
				FinalPTPredict$PT[i] <- PT_jobber
				FinalPTPredict$PTConfidence[i] <- ConJob
			} else{
				FinalPTPredict$PT[i] <- PT_IF
				FinalPTPredict$PTConfidence[i] <- ConIF
			}
		
		} else if(MatchValue == "DCI_IF_Present"){
			if(DiffDCI>DiffIF){
				FinalPTPredict$PT[i] <- PT_DCI
				FinalPTPredict$PTConfidence[i] <- ConDCI
			} else{
				FinalPTPredict$PT[i] <- PT_IF
				FinalPTPredict$PTConfidence[i] <- ConIF
			}
		
		} else{}
	}	

	###Process FinalSEPredict
	for(i in 1:nrow(FinalSEPredict)){
		MatchValue = FinalSEPredict$SEMatch[i]
		SE_jobber = FinalSEPredict$PredictedLabel_jobber[i]
		SE_IF = FinalSEPredict$PredictedLabel_IF[i]
		SE_DCI = FinalSEPredict$PredictedLabel_DCI[i]
		DiffJob = as.numeric(FinalSEPredict$Difference_jobber[i])
		DiffIF = as.numeric(FinalSEPredict$Difference_IF[i])
		DiffDCI = as.numeric(FinalSEPredict$Difference_DCI[i])
		ConJob = FinalSEPredict$EasyConfidence_jobber[i]
		ConIF = FinalSEPredict$EasyConfidence_IF[i]
		ConDCI = FinalSEPredict$EasyConfidence_DCI[i]

		if(MatchValue == "All_Match"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- "High"
		} else if(MatchValue == "No_Match"){
	
				if(DiffJob > DiffIF & DiffJob > DiffDCI){
					FinalSEPredict$SE[i] <- SE_jobber
					FinalSEPredict$SEConfidence[i] <- ConJob
				} else if(DiffIF > DiffJob & DiffIF > DiffDCI){
					FinalSEPredict$SE[i] <- SE_IF
					FinalSEPredict$SEConfidence[i] <- ConIF
				} else if(DiffDCI > DiffJob & DiffDCI> ConIF ){
					FinalSEPredict$SE[i] <- SE_DCI
					FinalSEPredict$SEConfidence[i] <- ConDCI
				} else{}

		} else if(MatchValue == "IF_Only"){
			FinalSEPredict$SE[i] <- SE_IF
			FinalSEPredict$SEConfidence[i] <- ConIF

		} else if(MatchValue == "DCI_Only"){
			FinalSEPredict$SE[i] <- SE_DCI
			FinalSEPredict$SEConfidence[i] <- ConDCI
		
		} else if(MatchValue == "Jobber_Only"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- ConJob
		
		} else if(MatchValue == "Jobber_DCI_Match"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "Jobber_IF_Match"){
			FinalSEPredict$SE[i] <- SE_jobber
			FinalSEPredict$SEConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "DCI_IF_Match"){
			FinalSEPredict$SE[i] <- SE_IF
			FinalSEPredict$SEConfidence[i] <- "High-Medium"
		
		} else if(MatchValue == "Jobber_DCI_Present"){
			if(DiffJob>DiffDCI){
				FinalSEPredict$SE[i] <- SE_jobber
				FinalSEPredict$SEConfidence[i] <- ConJob
			} else{
				FinalSEPredict$SE[i] <- SE_DCI
				FinalSEPredict$SEConfidence[i] <- ConDCI
			}
		
		} else if(MatchValue == "Jobber_IF_Present"){
			if(DiffJob>DiffIF){
				FinalSEPredict$SE[i] <- SE_jobber
				FinalSEPredict$SEConfidence[i] <- ConJob
			} else{
				FinalSEPredict$SE[i] <- SE_IF
				FinalSEPredict$SEConfidence[i] <- ConIF
			}
		
		} else if(MatchValue == "DCI_IF_Present"){
			if(DiffDCI>DiffIF){
				FinalSEPredict$SE[i] <- SE_DCI
				FinalSEPredict$SEConfidence[i] <- ConDCI
			} else{
				FinalSEPredict$SE[i] <- SE_IF
				FinalSEPredict$SEConfidence[i] <- ConIF
			}
		
		} else{}
	}

#Jobber
} else {

	#Make Prediction df via merging
	PredictPT_jobber = merge(MS_PTlabel, string_jobber, by = "Numb_Sku", all = TRUE)
	PredictSE_jobber = merge(MS_Serieslabel, string_jobber, by = "Numb_Sku", all = TRUE)

	#Make Prediction from prediction df
	PrePTResult_jobber = NBPredict(PredictionDF = PredictPT_jobber, source = "jobber")
	PreSEResult_jobber = NBPredict(PredictionDF = PredictSE_jobber, source = "jobber")

	#Subset result df
	PrePTResult_jobber = subset(PrePTResult_jobber, select=c("Numb_Sku", "PredictedLabel_jobber", "EasyConfidence_jobber", "Difference_jobber"))
	PreSEResult_jobber = subset(PreSEResult_jobber, select=c("Numb_Sku", "PredictedLabel_jobber", "EasyConfidence_jobber", "Difference_jobber"))

	#Rename FinalPT/SEPredict df
	names(PrePTResult_jobber) = c("Numb_Sku", "PT", "PTConfidence", "Difference")
	names(PreSEResult_jobber) = c("Numb_Sku", "SE", "SEConfidence", "Difference")

	#Merge to aquire FinalPTPredict df
	FinalPTPredict = PrePTResult_jobber
	FinalSEPredict = PreSEResult_jobber

}

###Merge Prediction data with Update Analysis
FinalPTPredict_Done = subset(FinalPTPredict, select = c("Numb_Sku", "PT", "PTConfidence"))
names(FinalPTPredict_Done) = c("Numb_Sku", "part_type_filter", "PTConfidence")

FinalSEPredict_Done = subset(FinalSEPredict, select = c("Numb_Sku", "SE", "SEConfidence"))
names(FinalSEPredict_Done) = c("Numb_Sku", "series_parent", "SEConfidence")


PredictedPT = merge(Update.PostPA.Merge, FinalPTPredict_Done, by = "Numb_Sku")
PredictedSE = merge(PredictedPT, FinalSEPredict_Done, by = "Numb_Sku")

CompletePrediction = PredictedSE

###############################################
###############################################


