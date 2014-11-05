best <- function(state, outcome) {
        ## Read outcome datahead and create lists of acceptable inputs
        data <- data.frame(read.csv("outcome-of-care-measures.csv", colClasses = "character"))
        state_list <- unique(data$State)
        oc_list <- c("heart attack", "heart failure", "pneumonia")
        
      
        ## Check that state and outcome are valid
        if (is.element(state, state_list) == FALSE) { stop("invalid state")}
        if (is.element(outcome, oc_list) == FALSE) { stop("invalid outcome")}
       
        
        bystate_data <- subset(data, "State" = state, select = 1:46)

        ## Return hospital name in that state with lowest 30-day death
       result <- which.min(state_data[,11])
       return(state_data[result,2])
}
