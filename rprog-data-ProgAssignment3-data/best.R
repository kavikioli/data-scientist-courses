best<-function (stateFromUS,output)
  ## The hospital name is the name provided in the Hospital.Name variable. 
  ## The outcomes can be one of \heart attack", \heart failure", or \pneumonia".
  ## Hospitals that do not have data on a particular outcome should be excluded 
  ## from the set of hospitals when deciding the rankings.
{
  
  
  ## Read outcome data
  outcomeDataFromHospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  stateList <- outcomeDataFromHospitals[,7] 
  
  if (output=="heart attack")
  {idxRow<-11 }
  else
  {
    if( output=="heart failure")
    { idxRow <-17}
    else
    { if(output == "pneumonia")
    {idxRow<-23}
    else
    {stop("Invalid outcome")}
    }
  }
  
  
  
  outcomeType <-   outcomeDataFromHospitals[,idxRow]
  hospitalNames <- outcomeDataFromHospitals[,2]
  #   stateList <-   outcomeDataFromHospitals[,idxRow]
  if (any(stateList==stateFromUS)==TRUE)
  {  
    findState <- stateList == stateFromUS
    
    outcomeTargetRaw <- suppressWarnings(as.numeric(outcomeType[findState])) 
    hospitalTargetRaw <- hospitalNames[findState]
    outcomeTarget<-outcomeTargetRaw[is.na(hospitalTargetRaw)==FALSE && is.na(outcomeTargetRaw)==FALSE]
#     hospitalTarget <- hospitalNames[findState]
    hospitalTarget<-hospitalTargetRaw[is.na(hospitalTargetRaw)==FALSE && is.na(outcomeTargetRaw)==FALSE]
    
    IDXoutcomeTargetClassif<-sort.int(outcomeTarget, na.last = NA, decreasing = FALSE, index.return = TRUE)
    
    outcomeTargetClassif<- IDXoutcomeTargetClassif$x
    outcomeTargetFirsts<-outcomeTargetClassif[IDXoutcomeTargetClassif$x[1]==outcomeTargetClassif]
    hospitalClassif <- hospitalTarget[IDXoutcomeTargetClassif$ix[IDXoutcomeTargetClassif$x[1]==outcomeTargetClassif]]
    hospitalSorted<- order(hospitalClassif)
    
    nameOfBestHospital <- hospitalClassif[hospitalSorted]
    
  }
  else
    
  {stop("Invalid state")}
  
}