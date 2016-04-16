rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    outcomeDfr <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    # --- Coerce character into numeric
    suppressWarnings(outcomeDfr[, 11] <- as.numeric(outcomeDfr[, 11]))
    suppressWarnings(outcomeDfr[, 17] <- as.numeric(outcomeDfr[, 17]))
    suppressWarnings(outcomeDfr[, 23] <- as.numeric(outcomeDfr[, 23]))
    
    # --- Create a data frame of freq by state Remove row.names
    tableDfr <- data.frame(State = names(tapply(outcomeDfr$State, outcomeDfr$State, 
                                                length)), Freq = tapply(outcomeDfr$State, outcomeDfr$State, length))
    rownames(tableDfr) <- NULL
    
    # --- Create a data frame of possible inputs and respective columns
    inputDfr <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
                           Col = c(11, 17, 23))
    
    # --- Check that outcome is valid
    if (nrow(inputDfr[inputDfr$Outcome == outcome, ]) == 0) 
        stop("invalid outcome")
    
    # --- Assert create an empty vector Add column rank for debug
    nameChr <- character(0)
    # rankChr <- character(0)
    
    # --- Return hospital name in that state with the ranked THIRTY(30)-day
    # death rate Create a data frame with given ONE (1) state Determine the
    # relevant column Reorder the new data frame from best to worst
    for (stateChr in tableDfr$State) {
        stateDfr <- outcomeDfr[outcomeDfr$State == stateChr, ]
        colNum <- inputDfr[inputDfr$Outcome == outcome, 2]
        stateDfr <- stateDfr[complete.cases(stateDfr[, colNum]), ]
        stateDfr <- stateDfr[order(stateDfr[, colNum], stateDfr$Hospital.Name), 
                             ]
        
        # --- Convert 'best' and 'worst' to numeric Determine the relevant row
        if (num == "best") 
            rankNum <- 1 else if (num == "worst") 
                rankNum <- nrow(stateDfr) else suppressWarnings(rankNum <- as.numeric(num))
        
        # --- Append hospital name to character vector
        nameChr <- c(nameChr, stateDfr[rankNum, ]$Hospital.Name)
        # rankChr <- c( rankChr, rankNum )
    }
    
    # --- Return value is a data frame (hospital, state)
    return(data.frame(hospital = nameChr, state = tableDfr$State))
}