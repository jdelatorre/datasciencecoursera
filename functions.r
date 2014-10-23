add2 <- function(x,y) {
        x + y
}

above10 <- function(x){
        use <- x>10
        x[use]
}

above <- function(x, n=10) {
        use <- x > n
        x[use]
}

columnmean <- function(y) {
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc) {
                means[i] <- mean(y[, i])
        }
        means
}

columnmean <- function(y, removeNA = TRUE) {
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc) {
                means[i] <- mean(y[, i], na.rm = removeNA)
        }
        means
}

weightmedian <- function(directory, day) {
        
        files_list <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        for (i in 1:5) {
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        dat_subset <- dat[which(dat[, "Day"] == day),]
        median(dat_subset[, "Weight"], na.rm = TRUE)
        
}

corr <- function(directory, threshold = 0) {

        files_list <- list.files(directory, full.names = TRUE)
        obs <- vector()
        for (i in 1:332) {

                if(sum(complete.cases(read.csv(files_list[i]))) > threshold) {
                        dat <- read.csv(files_list[i]) 
                        dat2 <- cor(dat[,2],dat[,3], "complete.obs")
                        obs <- c(obs, dat2)
                } 
        }
        return(as.vector(obs))
}



crazy <- function() {
        x <<- 3.14                   # variable x in the containing environment (global in this case) is updated to be 3.14
        print(x)                        # since no local variable 'x' exists within function 'crazy' R searches the containing environments
{ print(x);                     # this is to demonstrate the function, not a code block, is the smallest environment in R
  x <- 42; print(x)         # local variable 'x' is declared (created) and assigned the value 42; overrides the variable 'x' in
        }                                  # the containing environment
print(x)                       # since local variable 'x' now exists within the function there is no need to search the containing
}                                   # environment (global in this case)

crazy <- function() {                # create a new environment with a local variable 'x' and access to another variable 'x'
        # declared somewhere outside this function
        x <- 3.14                                # assign the numeric value 3.14 to local variable 'x'
        print(x)                                   # output the current value of local variable 'x' (1)
        
{ print(x);                                 # output the current value of local variable 'x' (2)
  x <<- 42;                              # assign the numeric value 42 to variable 'x' declared outside this function (3)
  print(x)                                 # output the current value of local variable 'x' (4)
        }
