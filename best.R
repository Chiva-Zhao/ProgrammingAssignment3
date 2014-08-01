best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
    stateData <- data[,7]
    if(!state %in% stateData)
        stop("invalid state")
    validOutcome <- c('heart attack','heart failure','pneumonia')
    
    if(!outcome %in% validOutcome)
        stop("invalid outcome")
    colname <- ''
    if(outcome == 'heart attack')
        colname = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    else if(outcome == 'heart failure')
        colname = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    else if(outcome == 'pneumonia')
        colname = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    data[,colname]<-as.numeric(data[,colname])
    data1<-subset(data,data[,'State'] == state)
    data2<-subset(data1,!is.na(data1[colname]))
    minValue = min(data2[,colname],na.rm=T)
    result1 = subset(data2,data2[,colname] == minValue)
    sort(result1$Hospital.Name)[1]
} 