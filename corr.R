corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        files_list <- list.files(directory, full.names = TRUE)
        dat <- vector()
        for (i in 1:332) {
             if(sum(complete.cases(read.csv(files_list[i]))) < threshold) {
                     dat <- c(dat, cor(dat[,"sulfates"],dat[,"nitrate"], "complete.obs"))
                     
             }
        }
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
}
