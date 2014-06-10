best <- function(state, outcome) {
    ## Read outcome data
    hospitaldata <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    ##hospitaldata[,c(11, 17, 23)] <- as.numeric(hospitaldata[,c(11, 17, 23)])
    hospitaldata[,11] <- as.numeric(hospitaldata[,11])
    
    
    ## Check that state and outcome are valid
    if (is.na(match(state, hospitaldata[,7]))){
        stop("invalid state")
    }    
    
    
    if (outcome == "heart attack"){
        index <- 11
    }else if (outcome == "heart failure"){
        index <- 17
    }else if (outcome == "pneumonia"){
        index <- 23
    }else{
        stop("invalid outcome")
    }
        
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate    
    #Another way:
    #minvalue <- min(subdata[, index], na.rm=TRUE)
    #namelist <- subset(subdata, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minvalue, select=Hospital.Name)
    
    subdata <- subset(hospitaldata, State == state)
    namelist <- subdata[which.min(subdata[,index]), 2]
    namelist <- sort(namelist)
    name <- as.character(namelist[1])
    name
}