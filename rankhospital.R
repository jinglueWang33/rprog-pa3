rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    hospitaldata <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    
    ## Check that state and outcome are valid
    if (is.na(match(state, hospitaldata[,7]))){
        stop("invalid state")
    }    
    
    
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
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    ## get subset we concerned
    subdata <- subset(hospitaldata, State == state, select=c(2,index))
    names(subdata) <- c("name","outcome")
    
    ## remove na
    good <- complete.cases(subdata[,2])
    subdata <- subdata[good,]
    
    ## sort dat
    orderdata <- subdata[order(subdata$outcome,subdata$name),]
    
    ## get name
    if (num == "best"){
        name <- orderdata[1,1]
    }else if(num == "worst"){
        name <- orderdata[dim(orderdata)[1],1]
    }else if (num < dim(orderdata)[1] & num > 0){
        name <- orderdata[num, 1]
    }else{
        name <- NA
    }
    
    name
}