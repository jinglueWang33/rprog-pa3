rankall <- function(outcome,num="best"){
    ## Read outcome data
    hospitaldata <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    
    ## Check that state and outcome are valid
    if (outcome == "heart attack"){
        index <- 11
        hospitaldata[,11] <- as.numeric(hospitaldata[,11])
    }else if (outcome == "heart failure"){
        index <- 17
        hospitaldata[,17] <- as.numeric(hospitaldata[,17])
    }else if (outcome == "pneumonia"){
        index <- 23
        hospitaldata[,23] <- as.numeric(hospitaldata[,23])
    }else{
        stop("invalid outcome")
    }
    
    ## For,each state, find the hospital of the given rank
    
    ## get subset and rename
    subdata <- hospitaldata[,c(2, 7, index)]
    names(subdata) <- c("name","state","outcome")
    
    ## remove NA values
    good <- complete.cases(subdata$state, subdata$outcome, subdata$name)
    subdata <- subdata[good, ]
    
    ## sort data
    sortdata <- subdata[order(subdata$state, subdata$outcome, subdata$name), ]
    
    
    ## Return a data frame with the hospital name and the 
    ## state name 
    if (num == "best"){
        hospital <- tapply(sortdata$name, sortdata$state, function(elt) elt[1])
        state <- tapply(sortdata$state, sortdata$state, function(elt) elt[1])
    }else if (num == "worst"){
        hospital <- tapply(sortdata$name, sortdata$state, function(elt) elt[length(elt)])
        state <- tapply(sortdata$state, sortdata$state, function(elt) elt[1])
    }else{
        hospital <- tapply(sortdata$name, sortdata$state, function(elt) elt[num])
        state <- tapply(sortdata$state, sortdata$state, function(elt) elt[1])
    }
    
    name_state <- as.data.frame(cbind(hospital, state))

    name_state    
}