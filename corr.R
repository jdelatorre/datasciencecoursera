corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        files_list <- list.files(directory, full.names = TRUE)
        obs <- vector()
        for (i in 1:332) {
                ## 'threshold' is a numeric vector of length 1 indicating the
                ## number of completely observed observations (on all
                ## variables) required to compute the correlation between
                ## nitrate and sulfate; the default is 0
                if(sum(complete.cases(read.csv(files_list[i]))) > threshold) {
                     dat <- read.csv(files_list[i]) 
                     dat2 <- cor(dat[,2],dat[,3], "complete.obs")
                     obs <- c(obs, dat2)
             } 
        }
        
        ## Return a numeric vector of correlations
        return(as.vector(obs))
}
