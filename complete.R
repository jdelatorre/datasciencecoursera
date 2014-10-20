complete <- function(directory, id = 1:332) {
        files_list <- list.files(directory, full.names = TRUE)
        nobs <- vector()
        for (i in id) {
                id_names <- c(id)
                nobs <- c(nobs, sum(complete.cases(read.csv(files_list[i]))))
                
        }
        dat <- cbind(id_names, nobs)
        colnames(dat) <- c("id", "nobs")
        return(data.frame(dat))
}
