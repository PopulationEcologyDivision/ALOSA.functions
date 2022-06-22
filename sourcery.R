
sourcery<-function(){
  setwd(choose.dir(caption = "Navigate to folder where functions are located"))
  filenames=list.files(path = getwd(), full.names = T)
  for (i in 1:length(filenames)){
    source(filenames[i])
  }
}

sourcery()
