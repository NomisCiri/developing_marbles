#kills futures that you have sent out.
#takes a vector of subprocesses as argument
#

# get all PIDs of the r processess
kill_futures<-function(works){
  v <- listenv::listenv()  # requires listenv package
  for (ii in 1:works) {
    v[[ii]] %<-% {
      Sys.getpid()
    }
  }
  
  for (i in 1:works) {
    #For windows
    system(sprintf("taskkill /F /PID %s", v[[i]]))
    
    #For Linux
    system(sprintf("kill -9 %s", v[[i]]))
  }
}