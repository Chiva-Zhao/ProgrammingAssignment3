rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    file <- "outcome-of-care-measures.csv"
    hospitalName <- 'Hospital.Name'
    data <- read.csv(file,colClasses = "character")
    states <- unique(data[,7])
    if(!state %in% states)
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
    data3<- data2[order(data2[,colname],data2[,hospitalName]),]
    if(is.character(num)){
        if(num == 'best'){
            data3[1,hospitalName]
        }else if(num == 'worst'){
            data3[nrow(data3),hospitalName]
        }
    }else if(is.numeric(num)){
        data3[num,hospitalName]
    }else
        NA 
        

    
}