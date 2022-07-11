
sourcery<-function(){
  setwd("~/git/ALOSA.functions/functions")
  filenames=list.files(path = getwd(), full.names = T)
  for (i in 1:length(filenames)){
    source(filenames[i])
  }
}


