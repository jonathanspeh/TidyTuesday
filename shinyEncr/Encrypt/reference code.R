# Reference code


encr<-function(x){
  x<-unlist(strsplit(x,split = ""))
  alp<-c(letters,LETTERS," ","'",".","!","?","Ä","ä","Ö","ö","Ü","ü",as.character(0:9))  
  set.seed(1)
  num1<-sample(1:length(alp),length(alp),replace = FALSE)
  df1<-data.frame(letter=alp,number=num1)
  set.seed(2)
  num2<-sample(1:length(alp),length(alp),replace = FALSE)
  df2<-data.frame(number=num2,letter=alp)
  index<-c()
  for(i in seq_along(x)){
    index[i]<-df1%>%filter(letter==x[i])%>%select(number)
    df1<-data.frame(letter=alp,number=c(num1[2:length(num1)],num1[1]))
  }
  output<-c()
  for(i in seq_along(index)){
    output[i]<-df2%>%filter(number==index[i])%>%select(letter)
    df2<-data.frame(number=c(num2[1:length(num2)-1],num2[length(num2)]),letter=alp)
  }
  return(paste0(as.character(unlist(output)),collapse = ""))
}



decr<-function(x){
  if(is.character(x)==TRUE){
    x<-unlist(strsplit(x,split = ""))
  }
  alp<-c(letters,LETTERS," ","'",".","!","?","Ä","ä","Ö","ö","Ü","ü",as.character(0:9))
  set.seed(1)
  num1<-sample(1:length(alp),length(alp),replace = FALSE)
  df1<-data.frame(letter=alp,number=num1)
  set.seed(2)
  num2<-sample(1:length(alp),length(alp),replace = FALSE)
  df2<-data.frame(number=num2,letter=alp)
  index<-c()
  for(i in seq_along(x)){
    index[i]<-df2%>%filter(letter==x[i])%>%select(number)
    df2<-data.frame(number=c(num2[1:length(num2)-1],num2[length(num2)]),letter=alp)
  }
  output<-c()
  for(i in seq_along(index)){
    output[i]<-df1%>%filter(number==index[i])%>%select(letter)
    df1<-data.frame(number=c(num1[2:length(num1)],num1[1]),letter=alp)
  }
  return(paste0(as.character(unlist(output)),collapse = ""))
}
  


encr("tttttttt")->code

decr(code)





paste0(c("A","B"),collapse = "")

library(shiny)
runExample("06_tabsets")


