# Works so far
# TODO: Make action buttons useful,
#       figure out what is wrong with long texts (i.e. >27 charcaters?)



library(shiny)
library(dplyr)


# define custom funnctions
encr<-function(x,pcode){
    x<-unlist(strsplit(x,split = ""))
    alp<-c(letters,LETTERS," ","'",".","!","?","Ä","ä","Ö","ö","Ü","ü",as.character(0:9))  
    set.seed(pcode)
    num1<-sample(1:length(alp),length(alp),replace = FALSE)
    df1<-data.frame(letter=alp,number=num1)
    set.seed(2)
    num2<-sample(1:length(alp),length(alp),replace = FALSE)
    df2<-data.frame(number=num2,letter=alp)
    set.seed(3)
    num3<-sample(1:length(alp),length(alp),replace = FALSE)
    set.seed(4)
    num4<-sample(1:length(alp),length(alp),replace = FALSE)
    df3<-data.frame(inp=num3,outp=num4)
    index<-c()
    for(i in seq_along(x)){
        index[i]<-df1%>%filter(letter==x[i])%>%select(number)
        df1<-data.frame(letter=alp,
                        number=c(df1$number[2:length(num1)],df1$number[1]))
    }
    output<-c()
    for(i in seq_along(index)){
        output[i]<-df2%>%filter(number==index[i])%>%select(letter)
        df2<-data.frame(number=c(df2$number[2:length(num2)-1],df2$number[1]),
                        letter=alp)
    }
    return(paste0(as.character(unlist(output)),collapse = ""))
}



decr<-function(x,pcode){
    if(is.character(x)==TRUE){
        x<-unlist(strsplit(x,split = ""))
    }
    alp<-c(letters,LETTERS," ","'",".","!","?","Ä","ä","Ö","ö","Ü","ü",as.character(0:9))
    set.seed(pcode)
    num1<-sample(1:length(alp),length(alp),replace = FALSE)
    df1<-data.frame(letter=alp,number=num1)
    set.seed(2)
    num2<-sample(1:length(alp),length(alp),replace = FALSE)
    df2<-data.frame(number=num2,letter=alp)
    set.seed(3)
    num3<-sample(1:length(alp),length(alp),replace = FALSE)
    set.seed(4)
    num4<-sample(1:length(alp),length(alp),replace = FALSE)
    df3<-data.frame(inp=num3,outp=num4)
    index<-c()
    for(i in seq_along(x)){
        index[i]<-df2%>%filter(letter==x[i])%>%select(number)
        df2<-data.frame(number=c(df2$number[2:length(num2)-1],df2$number[1]),
                        letter=alp)
    }
    output<-c()
    for(i in seq_along(index)){
        output[i]<-df1%>%filter(number==index[i])%>%select(letter)
        df1<-data.frame(letter=alp,
                        number=c(df1$number[2:length(num1)],df1$number[1]))
    }
    return(paste0(as.character(unlist(output)),collapse = ""))
}

# Define UI 
ui <- fluidPage(
    # Application title
    titlePanel("Write a secret message"),

    # Input for clear and crypt text
    sidebarLayout(
        sidebarPanel(
            textInput("clear",
                      label = "Enter your message in clear text",
                      value = ""),
            textInput("crypt",
                      label = "Enter your encoded message",
                      value = ""),
            numericInput("pin",
                      label = "Enter your pincode",
                      value = 1),
            actionButton(inputId = "encrypt",
                         label = "encrypt!"),
            actionButton(inputId = "decrypt",
                         label = "decrypt!")),

        # Show output
        mainPanel(
            h2("Your code"),
            textOutput("code"),
            h2("You message"),
            textOutput("message")
        )
    )
)

# Define server logic 
server <- function(input, output) {
    output$code<-renderText({encr(input$clear,input$pin)})
    output$message<-renderText({decr(input$crypt,input$pin)})
}
# Run the application 
shinyApp(ui = ui, server = server)
