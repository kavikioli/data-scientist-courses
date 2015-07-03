rankhospital<-function (stateFromUS,output,num="best")
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
    outcomeTarget<-outcomeTargetRaw[is.na(hospitalTargetRaw)==FALSE]
    outcomeTarget <- outcomeTarget[is.na(outcomeTarget)==FALSE]

    #     hospitalTarget <- hospitalNames[findState]
    hospitalTarget <-hospitalTargetRaw[is.na(hospitalTargetRaw)==FALSE]
    hospitalTarget <- hospitalTarget[is.na(outcomeTarget)==FALSE]
    
    IDXoutcomeTargetClassif<-sort.int(outcomeTarget, na.last = NA, decreasing = FALSE, index.return = TRUE)
    
    if(num=="best")
    {  
      outcomeTargetClassif<- IDXoutcomeTargetClassif$x
      #     outcomeTargetFirsts<-outcomeTargetClassif[IDXoutcomeTargetClassif$x[1]==outcomeTargetClassif]
      hospitalClassif <- hospitalTarget[IDXoutcomeTargetClassif$ix]#[IDXoutcomeTargetClassif$x[1]==outcomeTargetClassif]]
      
      hospitalSorted<- order(hospitalClassif)
      nameOfBestHospital<-hospitalClassif[hospitalSorted]
      return(as.character(nameOfBestHospital[1]))     
    }
    else
    {      
      if(num=="worst")
      {

        outcomeTargetClassif<- IDXoutcomeTargetClassif$x
        sizeofList<-length(IDXoutcomeTargetClassif$x)
        outcomeTargetFirsts<-outcomeTargetClassif[IDXoutcomeTargetClassif$x[sizeofList]==outcomeTargetClassif]
        hospitalClassif <- hospitalTarget[IDXoutcomeTargetClassif$ix[IDXoutcomeTargetClassif$x[sizeofList]==outcomeTargetClassif]]
        
        hospitalSorted<- order(hospitalClassif)
        nameOfBestHospital<-hospitalClassif[hospitalSorted]
        return(as.character(nameOfBestHospital[1]))     
      }
      else
        if (num >length(IDXoutcomeTargetClassif$ix))
        {return(NA)}
      else
      {  
        outcomeTargetClassif<- IDXoutcomeTargetClassif$x
        if (all(diff(IDXoutcomeTargetClassif$x[1:num])>0))
        {
          outcomeTargetFirsts<-outcomeTargetClassif[IDXoutcomeTargetClassif$x[num]==outcomeTargetClassif]
          hospitalClassif <- hospitalTarget[IDXoutcomeTargetClassif$ix[IDXoutcomeTargetClassif$x[num]==outcomeTargetClassif]]
          
        }
         else
          {
            idxEqualTrue <-diff(IDXoutcomeTargetClassif$x[1:num])==0
            idxEqualTrueLog <-idxEqualTrue==TRUE
            newLastIdxOfTheVector <- num + sum(idxEqualTrueLog)
            outcomeTargetFirsts<-outcomeTargetClassif[IDXoutcomeTargetClassif$x[newLastIdxOfTheVector]==outcomeTargetClassif]
            hospitalClassif <- hospitalTarget[IDXoutcomeTargetClassif$ix[IDXoutcomeTargetClassif$x[newLastIdxOfTheVector]==outcomeTargetClassif]]
            
          }
          hospitalSorted<- order(hospitalClassif)
          nameOfBestHospital<-hospitalClassif[hospitalSorted]
#         sizeofList <-length(nameOfBestHospital)
        return(as.character(nameOfBestHospital[1]))     
      }
    }
  }
  else 
  {stop("Invalid state")
  }
  
}





#       {IDXoutcomeTargetClassif<-sort.int(outcomeTarget, na.last = NA, decreasing = FALSE, index.return = TRUE)
#       
#     outcomeTargetClassif<- IDXoutcomeTargetClassif$x
#     outcomeTargetFirsts<-outcomeTargetClassif[IDXoutcomeTargetClassif$x
#     hospitalClassif <- hospitalTarget[IDXoutcomeTargetClassif$ix]
#     
#     hospitalOfSameValueiDX <- diff(outcomeTargetFirsts)==0
#     outcomeTargetFirstsIdx<- order(outcomeTargetFirsts)
#     hospitalSorted<- order(hospitalClassif[hospitalOfSameValueiDX])
#     
#     nameOfBestHospital <- hospitalClassif[hospitalSorted]
#     rankOfOutcome <- outcomeTargetFirsts[outcomeTargetFirstsIdx]
#     return(as.character(nameOfBestHospital[1]))#,rankOfOutcome,1:numberOfHospitals),numberOfHospitals,3))
#     