## function which converts values of columns in a dataframe 
## to numeric by a given vector of names and hides warnings
toNumeric <- function(data, paths){
        for(path in paths){
                data[, path] <- 
                        suppressWarnings(as.numeric(data[, path]))
        }
}

## best hospital
best <- function(state, outcome) {
        ## Read outcome data
        measures <- read.csv("outcome-of-care-measures.csv", 
                             colClasses = "character")
        ## Set paths
        heart_attack <-
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        heart_failure <-
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        pneumonia <-
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        
        ## Paths into a vector
        outcomes <- c(heart_attack, heart_failure, pneumonia)
        
        ## Convert to numeric 
        toNumeric(measures, outcomes)

        ## Set vector of outcome names
        names(outcomes) <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if(!state %in% measures$State) {
                stop("invalid state")
        } else if (is.na(outcomes[as.character(outcome)])) {
                stop("invalid outcome")
        } else {
                state_hospitals <- measures[measures$State == state, ]
                
                ## Set a proper column to grab data
                number <- which(colnames(state_hospitals) == 
                                        as.character(outcomes[outcome]))
                
                ## Set a proper column to grab data
                ## out_numbers <- as.numeric(state_hospitals[, number])
                out_numbers <- suppressWarnings(as.numeric(
                        state_hospitals[, number]))
                        
                ## Find the minimum value
                minimal <- min(out_numbers, na.rm = TRUE)
                        
                ## Find the exact place where minimal value occurs
                index <- which(out_numbers == minimal)
                
                ## Get hospital name
                hospital <- state_hospitals[index, 2]
        }
        return(hospital)
}
