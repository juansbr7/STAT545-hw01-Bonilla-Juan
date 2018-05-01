## clean out any previous work
outputs <- c("file1.R",     # get the data
             "file2.R",     # clean data
             "file3.R",     # plot data
             list.files(pattern = "*.png$"))


## run my scripts
source("file1.R")
source("file2.R")
source("file3.R")