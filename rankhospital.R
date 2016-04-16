rankhospital <- function(state, outcome, num = "best") {
    data = read.csv("outcome-of-care-measures.csv",colClasses = "character")
    if (state %in% data$State & 
        outcome %in% c("heart failure","heart attack","pneumonia")){
        
        if(outcome=="heart attack"){
            mortData = data[,c(2,7,11)]
        }
        
        if(outcome=="heart failure"){
            mortData = data[,c(2,7,17)]
        }
        
        if(outcome=="pneumonia"){
            mortData = data[,c(2,7,23)]
        }
        
        mortDataByState = split(mortData,mortData$State)
        finalMortData = mortDataByState[[state]]
        
        finalMortData[,3] = as.numeric(finalMortData[,3])
        finalMortData = finalMortData[!is.na(finalMortData[,3]),]
        rank = as.numeric(num)
        if(is.na(rank)){
            if(num == "best"){
                output = finalMortData[order(finalMortData[,3],finalMortData[,1],na.last = TRUE),][1,1]
            } 
            else if (num == "worst"){
                output = finalMortData[order(finalMortData[,3],finalMortData[,1],na.last = TRUE,decreasing = TRUE),][1,1]
            }
            else{
                stop("Invalid rank")
            }
        }
        else{
            if(length(data) < rank){
                output = NA
            }
            else{
                output = finalMortData[order(finalMortData[,3],finalMortData[,1],na.last = TRUE),][num,1]
            }
        }
        
        bestHospital = finalMortData[order(finalMortData[,3],finalMortData[,1],na.last = TRUE),][1,1]
    }
    else if(state %in% data$State == FALSE){
        stop("Invalid state")
    }
    else if(outcome %in% c("heart failure","heart attack","pneumonia")){
        stop("Invalid outcome")
    }
output
}