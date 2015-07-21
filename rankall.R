rankall<-function(outcome,num="best"){
  
  
  
  outcomedata<-read.csv("outcome-of-care-measures.csv", na.strings="Not Available", colClasses="character")
  
  #library plyr 
  library(plyr)
  
  ##turn mortality rates into numbers for sorting
  outcomedata[,11]<-as.numeric(outcomedata[,11])
  outcomedata[,17]<-as.numeric(outcomedata[,17])
  outcomedata[,23]<-as.numeric(outcomedata[,23])
  
  
  ##Check that state and outcome are valid
  
  #create a vector of state names from the list
  statetest<-unique(outcomedata[,7])
  
  
  
  
  #create the name vector
  conditions<-c('heart attack','heart failure','pneumonia')
  logicaltest_outcome<-is.element(outcome, conditions)
  
  if(logicaltest_outcome==FALSE) {
    stop("invalid outcome")
  }
  
  
  else if(outcome == "heart attack") {
    
    ha_data<-subset(outcomedata, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    
    #clean up column names, create tidy set new_ha_data
    new_ha_data<-rename(ha_data, c("Hospital.Name"="hospital", "State" ="state", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"="MortalityRate"))
    
    #sort the tidy set, store as ha_data_sorted
    ha_data_sorted<-arrange(new_ha_data, MortalityRate, hospital)
    
    rankedData<-transform(ha_data_sorted, Rank = ave(MortalityRate, state, FUN = function(Rank) order(Rank, decreasing=F)))
    
    #remove NA
    rankedData1<-rankedData[complete.cases(rankedData),]
    
    #account for best or worst
    if(num=="best"){
      num<-1
    }
    if(num=="worst"){
      num<-nrow(rankedData1)
    }
    #Rank the hospitals
    rankedData2<-subset(rankedData1,Rank==num)
    #arrange them by state
    rankedData3<-arrange(rankedData2, state)
    #return result
    print(rankedData3[,1:2])
  }
  
  
  ####if outcome==heart failure
  
  else if(outcome == "heart failure"){
    
    hf_data<-subset(outcomedata, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    
    #clean up column names, create tidy set new_hf_data
    new_hf_data<-rename(hf_data, c("Hospital.Name"="hospital", "State"="state", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" = "MortalityRate"))
    
    #sort the tidy set, store as ha_data_sorted
    hf_data_sorted<-arrange(new_hf_data, MortalityRate, hospital)
    
    #create ranking
    rankedData<-transform(hf_data_sorted, Rank = ave( MortalityRate, state, FUN = function(Rank) order(Rank, decreasing=F)))
    
    #remove NA
    rankedData1<-rankedData[complete.cases(rankedData),]
    
    #account for best or worst
    if(num=="best"){
      num<-1
    }
    if(num=="worst"){
      num<-nrow(rankedData1)
    }
    
    #Rank the hospitals
    rankedData2<-subset(rankedData1,Rank==num)
    #arrange them by state
    rankedData3<-arrange(rankedData2, state)
    #return result
    print(rankedData3[,1:2])
    }
  
  ####if outcome==pneumonia
  
  else if(outcome=="pneumonia"){
    
    p_data<-subset(outcomedata, select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    #clean up column names, create tidy set new_p_data
    new_p_data<-rename(p_data, c("Hospital.Name"="hospital", "State"="state", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" = "MortalityRate"))
    
    #sort the tidy set, store as ha_data_sorted
    p_data_sorted<-arrange(new_p_data, MortalityRate, hospital)
    
    #create ranking
    rankedData<-transform(p_data_sorted, Rank = ave( MortalityRate, state, FUN = function(Rank) order(Rank, decreasing=F)))
    
    #remove NA
    rankedData1<-rankedData[complete.cases(rankedData),]
    
    
    
    #account for best or worst
    if(num=="best"){
      num<-1
    }
    if(num=="worst"){
     
     num<-200
    }
    
    #Rank the hospitals
    rankedData2<-subset(rankedData1,Rank==num)
    #arrange them by state
    rankedData3<-arrange(rankedData2, state)
    #return result
    print(rankedData3[,1:2])
  }
}