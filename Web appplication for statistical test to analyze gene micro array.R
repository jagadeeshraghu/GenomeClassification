library(e1071)
library(shiny)
library(caret)
library(infotheo)
shinyUI<-
  fluidPage(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    titlePanel(
      h3("WEB APPLICATION FOR STATISTICAL TEST")),hr(),
    sidebarLayout(
      sidebarPanel(
        actionButton(inputId = "button", label = "Data Set"),hr(),br(),
        actionButton(inputId = "button1", label = "SVM alonds"),
        actionButton(inputId = "button2", label = "SVM lymph"),hr(),br(),
        actionButton(inputId = "button3", label = "(SVM)Correlation-Coefficient alonds"),
        actionButton(inputId = "button4", label = "(SVM)Correlation-Coefficient lymph"),hr(),br(), 
        actionButton(inputId = "button5", label = "(SVM)Information-Gain alonds"),
        actionButton(inputId = "button6", label = "(SVM)Information-Gain lymph"),hr(),br(),
        actionButton(inputId = "button7", label = "(SVM)Mutual-Information alonds"),
        actionButton(inputId = "button8", label = "(SVM)Mutual-Information lymph"),hr(),br(),
        actionButton(inputId = "button9", label = "FULL PLOT"),
        actionButton(inputId = "button10", label = "ALONDS PLOT"),
        actionButton(inputId = "button11", label = "LYMPH PLOT")),
      mainPanel(
        tableOutput(outputId = 'table.output'),hr(),br(),
        tableOutput(outputId = 'table.output2'),hr(),br(),
        tableOutput(outputId = 'table.output1'),hr(),br(),
        tableOutput(outputId = 'table.output3'),hr(),br(),
        tableOutput(outputId = 'table.output4'),hr(),br(),
        tableOutput(outputId = 'table.output5'),hr(),br(),
        tableOutput(outputId = 'table.output6'),hr(),br(),
        tableOutput(outputId = 'table.output7'),hr(),br(),
        tableOutput(outputId = 'table.output8'),
        plotOutput("table.output9"),
        plotOutput("table.output10"),
        plotOutput("table.output11")
      )
    )
  )
shinyserver<-function(input, output) {
  syn1 <- function() {
    getwd()
    setwd("C:/Users/ELCOT-Lenovo/Music/papaer")
    getwd()
    s <- read.csv("alonds.csv",header = TRUE)
    xt=s[1:50,c(1:2001)]
    zt=s[51:62,c(1:2001)]
    yt=s[1:50,2002]
    at=s[51:62,2002]
    b<-svm(yt~.,data=xt)                                #original svm
    pred <- predict(b,zt)
    t=confusionMatrix(pred,at)
    a=t(as.matrix(t,what="classes"))
  }
  syn2 <- function() {
    getwd()
    setwd("C:/Users/ELCOT-Lenovo/Music/papaer")
    getwd()
    s <- read.csv("lymp.csv",header = TRUE)
    xt=s[1:153,1:2077]
    zt=s[154:190,1:2077]
    yt=s[1:153,2078]
    at=s[154:190,2078]
    b<-svm(yt~.,data=xt)
    pred <- predict(b,zt)
    t=confusionMatrix(pred,at)
    a<-t(as.matrix(t,what="classes"))
  }
  syn3 <- function() {
    getwd()
    setwd("C:/Users/ELCOT-Lenovo/Music/papaer")
    getwd()
    s <- read.csv("alonds.csv",header = TRUE)
    xt=s[1:50,c(1:2001)]
    zt=s[51:62,c(1:2001)]
    yt=s[1:50,2002]
    at=s[51:62,2002]
    x=discretize(s[1:50,c(1:2001)])
    y=discretize(s[1:50,2002])
    r=cbind(x,y)  
    m=cor(r,method="kendall",use="everything") 
    k=NULL
    k=m[1:2001,2002]
    l=as.matrix(k)
    res2=NULL
    j=0
    j=min(l)+max(l)
    j=j/2
    for(i in 1:length(l))
    {
      if(l[i]>j)
      {
        res2=c(res2,i)
      }
    }
    length(res2)
    y=NULL
    y=s[,res2]
    res2
    b<-svm(s$grouping~.,data=y)
    pred <- predict(b,zt)
    t=confusionMatrix(pred,at)
    a<-t(as.matrix(t,what="classes"))
  }
  syn4 <- function() {
    getwd()
    setwd("C:/Users/ELCOT-Lenovo/Music/papaer")
    getwd()
    s <- read.csv("lymp.csv",header = TRUE)
    xt=s[1:153,1:2077]
    zt=s[154:190,1:2077]
    yt=s[1:153,2078]
    at=s[154:190,2078]
    x=discretize(s[1:153,1:2077])
    y=discretize(s[1:153,2078])
    r=cbind(x,y)
    m=cor(r,method="kendall",use="everything")
    k=NULL
    k=m[1:2077,2078]
    l=as.matrix(k)
    res2=NULL
    j=0
    j=min(l)+max(l)
    j=j/2
    for(i in 1:length(l))
    {
      if(l[i]>j)
      {
        res2=c(res2,i)
      }
    }
    length(res2)
    y=0
    y=s[,res2]
    b<-svm(s$Status~.,data=y)
    pred <- predict(b,zt)
    t=confusionMatrix(pred,at)
    a<-t(as.matrix(t,what="classes"))
  }
  syn5 <- function() {
    getwd()
    setwd("C:/Users/ELCOT-Lenovo/Music/papaer")
    getwd()
    s <- read.csv("alonds.csv",header = TRUE)
    info.gain <- function(partitionby, categories){
      partition.table <- table(partitionby, categories)
      row.sums <- as.vector(rowSums(partition.table))
      prob <- partition.table / row.sums
      ent <- -prob * log(prob, 2)
      ent[is.na(ent)] <- 0
      info.gain <- entropy(categories) - sum(ent * (row.sums / sum(row.sums)))
      return(info.gain)
    } 
    xt=s[1:50,c(1:2001)]
    zt=s[51:62,c(1:2001)]
    yt=s[1:50,2002]
    at=s[51:62,2002]
    x=discretize(s[1:50,c(1:2001)])
    y=discretize(s[1:50,2002])
    r=cbind(x,y)  
    v=NULL
    for(i in 1:2001)
    {
      v=c(v,info.gain(s[1:50,i],s[1:50,2002]))
    }
    l=as.matrix(v)
    l
    res2=NULL
    j=0
    j=min(l)+max(l)
    j=j/2
    for(i in 1:length(l))
    {
      if(l[i]>j)
      {
        res2=c(res2,i)
      }
    }
    length(res2)
    y=s[,res2]
    b<-svm(s$grouping~.,data=y)
    pred <- predict(b,zt)
    t=confusionMatrix(pred,at)
    t
    a<-t(as.matrix(t,what="classes"))
  }
  syn6 <- function() {
    getwd()
    setwd("C:/Users/ELCOT-Lenovo/Music/papaer")
    getwd()
    s <- read.csv("lymp.csv",header = TRUE)
    xt=s[1:153,1:2077]
    zt=s[154:190,1:2077]
    yt=s[1:153,2078]
    at=s[154:190,2078]
    x=discretize(s[1:153,1:2077])
    y=discretize(s[1:153,2078])
    r=cbind(x,y)
    info.gain <- function(partition.by, categories){
      partition.table <- table(partition.by, categories)
      row.sums <- as.vector(rowSums(partition.table))
      prob <- partition.table / row.sums
      ent <- -prob * log(prob, 2)
      ent[is.na(ent)] <- 0
      info.gain <- entropy(categories) - sum(ent * (row.sums / sum(row.sums)))
      return(info.gain)
    }
    v=NULL
    for(i in 1:2077)
    {
      v=c(v,info.gain(s[1:153,i],s[1:153,2078]))
    }
    l=as.matrix(v)
    res2=NULL
    j=0
    j=min(l)+max(l)
    j=j/2
    j
    for(i in 1:length(l))
    {
      if(l[i]>j)
      {
        res2=c(res2,i)
      }
    }
    length(res2)
    y=s[,res2]
    b<-svm(s$Status~.,data=y)
    pred <- predict(b,zt)
    t=confusionMatrix(pred,at)
    a<-t(as.matrix(t,what="classes"))
  }
  syn7 <- function() {
    getwd()
    setwd("C:/Users/ELCOT-Lenovo/Music/papaer")
    getwd()
    s <- read.csv("alonds.csv",header = TRUE)
    xt=s[1:50,c(1:2001)]
    zt=s[51:62,c(1:2001)]
    yt=s[1:50,2002]
    at=s[51:62,2002]
    x=discretize(s[1:50,c(1:2001)])
    y=discretize(s[1:50,2002])
    r=cbind(x,y)  
    m=mutinformation(r,method="sg")
    k=NULL
    k=I[1:2001,2002]
    l=as.matrix(k)
    res2=NULL
    j=0
    j=min(l)+max(l)
    j=j/2
    for(i in 1:length(l))
    {
      if(l[i]>j)
      {
        res2=c(res2,i)
      }
    }
    length(res2)
    y=NULL
    y=s[,res2]
    res2
    b<-svm(s$grouping~.,data=y)
    pred <- predict(b,zt)
    t=confusionMatrix(pred,at)
    a<-t(as.matrix(t,what="classes"))    
  }
  syn8 <- function() {
    getwd()
    setwd("C:/Users/ELCOT-Lenovo/Music/papaer")
    getwd()
    s <- read.csv("lymp.csv",header = TRUE)
    xt=s[1:153,1:2077]
    zt=s[154:190,1:2077]
    yt=s[1:153,2078]
    at=s[154:190,2078]
    x=discretize(s[1:153,1:2077])
    y=discretize(s[1:153,2078])
    r=cbind(x,y)
    m=mutinformation(r,method="sg")
    k=NULL
    k=m[1:2077,2078]
    l=as.matrix(k)
    res2=NULL
    j=0
    j=min(l)+max(l)
    j=j/2
    for(i in 1:length(l))
    {
      if(l[i]>j)
      {
        res2=c(res2,i)
      }
    }
    length(res2)
    y=s[,res2]
    b<-svm(s$Status~.,data=y)
    pred <- predict(b,zt)
    t=confusionMatrix(pred,at)
    a<-t(as.matrix(t,what="classes"))
  }
  va<-reactiveValues(a=0,b=0,c=0,d=0,e=0,f=0,g=0,h=0,i=0,j=0,k=0,l=0)
  observeEvent(input$button,{
    va$a<-1
    va$b<-0
    va$c<-0
    va$d<-0
    va$e<-0
    va$f<-0
    va$g<-0
    va$h<-0
    va$i<-0
    va$j<-0
    va$k<-0
    va$l<-0
  })
  observeEvent(input$button1,{
    va$a<-0
    va$b<-1
    va$c<-0
    va$d<-0
    va$e<-0
    va$f<-0
    va$g<-0
    va$h<-0
    va$i<-0
    va$j<-0
    va$k<-0
    va$l<-0
  })
  observeEvent(input$button2,{
    va$a<-0
    va$b<-0
    va$c<-1
    va$d<-0
    va$e<-0
    va$f<-0
    va$g<-0
    va$h<-0
    va$i<-0
    va$j<-0
    va$k<-0
    va$l<-0
  })
  observeEvent(input$button3,{
    va$a<-0
    va$b<-0
    va$c<-0
    va$d<-1
    va$e<-0
    va$f<-0
    va$g<-0
    va$h<-0
    va$i<-0
    va$j<-0
    va$k<-0
    va$l<-0
  })
  observeEvent(input$button4,{
    va$a<-0
    va$b<-0
    va$c<-0
    va$d<-0
    va$e<-1
    va$f<-0
    va$g<-0
    va$h<-0
    va$i<-0
    va$j<-0
    va$k<-0
    va$l<-0
  })
  observeEvent(input$button5,{
    va$a<-0
    va$b<-0
    va$c<-0
    va$d<-0
    va$e<-0
    va$f<-1
    va$g<-0
    va$h<-0
    va$i<-0
    va$j<-0
    va$k<-0
    va$l<-0
  })
  observeEvent(input$button6,{
    va$a<-0
    va$b<-0
    va$c<-0
    va$d<-0
    va$e<-0
    va$f<-0
    va$g<-1
    va$h<-0
    va$i<-0
    va$j<-0
    va$k<-0
    va$l<-0
  })
  observeEvent(input$button7,{
    va$a<-0
    va$b<-0
    va$c<-0
    va$d<-0
    va$e<-0
    va$f<-0
    va$g<-0
    va$h<-1
    va$i<-0
    va$j<-0
    va$k<-0
    va$l<-0
  })
  observeEvent(input$button8,{
    va$a<-0
    va$b<-0
    va$c<-0
    va$d<-0
    va$e<-0
    va$f<-0
    va$g<-0
    va$h<-0
    va$i<-1
    va$j<-0
    va$k<-0
    va$l<-0
  })
  observeEvent(input$button9,{
    va$a<-0
    va$b<-0
    va$c<-0
    va$d<-0
    va$e<-0
    va$f<-0
    va$g<-0
    va$h<-0
    va$i<-0
    va$j<-1
    va$k<-0
    va$l<-0   
  })
  observeEvent(input$button10,{
    va$a<-0
    va$b<-0
    va$c<-0
    va$d<-0
    va$e<-0
    va$f<-0
    va$g<-0
    va$h<-0
    va$i<-0
    va$j<-0
    va$k<-1
    va$l<-0
  })
  observeEvent(input$button11,{
    va$a<-0
    va$b<-0
    va$c<-0
    va$d<-0
    va$e<-0
    va$f<-0
    va$g<-0
    va$h<-0
    va$i<-0
    va$j<-0
    va$k<-0
    va$l<-1
  })
  observeEvent(input$button9,{
    output$ts <-renderUI({
      includeHTML(htmlTemplate ( filename  ="C:/Users/ELCOT-Lenovo/Desktop/review 1/button.html"))})
  }
  )
  output$table.output <- renderTable({
    if(va$a)
    {
      inFile <- input$file1
      m=(ncol(inFile))
      if (is.null(inFile))
        return(NULL)
      tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep,  dec = input$dec)
      return(tbl)
    }})
  output$table.output1<- renderTable({
    if(va$b)
    {
      data1<-(syn1())
      return(data1)
    }})
  output$table.output2<- renderTable({
    if(va$c)
    {
      data2<-syn2()
      return(data2)
    }})
  output$table.output3<- renderTable({
    if(va$d)
    {
      if(exists("table.output3"))
        data3<-table.output3
      else
        data3<-(syn3())
      return(data3)
    }})
  output$table.output4<- renderTable({
    if(va$e)
    {
      data4<-syn4()
      return(data4)
    }})
  output$table.output5 <- renderTable({
    if(va$f)
    {   
      data5<-(syn5())
      return(data5)
    }})
  output$table.output6 <- renderTable({
    if(va$g)
    {
      data6<-syn6()
      return(data6)
    }})
  output$table.output7 <- renderTable({
    if(va$h)
    {
      data7<-syn7()
      return(data7)
    }})
  output$table.output8<- renderTable({
    if(va$i)
    {
      data8<-syn8()
      return(data8)
    }
  })
  output$table.output9 <- renderPlot ({
    if(va$j)
    {
      acc1<-c(75,83.33,91.76,83.33)
      acc2<-c(70.27,97.30,91.89,97.30)
      sens1<-c(85.71,100,100,100)
      sens2<-c(27.273,90.91,72.73,90.91)
      spe1<-c(60,60,80,60)
      spe2<-c(88.462,100,100,100)    
      v<-cbind(acc1,acc2,sens1,sens2,spe1,spe2)
      barplot(v,beside = TRUE,col = c("red","green","blue","orange"),names.arg  = c("accuAl","acculy","sensal","sensly","special","specily"),xlab = "datasets",ylab="performance measures")
    }})
  output$table.output10 <- renderPlot ({
    if(va$k)
    {
      svm<-c(75,85.71,60)
      coef<-c(83.33,100,60)
      mut<-c(91.67,100,80)
      info<-c(83.33,100,60)
      
      v<-rbind(svm,coef,mut,info)
      barplot(v,beside = TRUE,col = c("red","green","blue","orange"),names.arg  = c("Accuracy","sensitivity","pecificity"),xlab = "ALONDS",ylab="performance measures")
    }})
  output$table.output11 <- renderPlot ({
    if(va$l)
    {
      svm<-c(70.27,27.273,88.462)
      coef<-c(97.30,90.91,100)
      mut<-c(91.89,72.273,100)
      info<-c(97.30,90.91,100)
      v<-rbind(svm,coef,mut,info)
      barplot(v,beside = TRUE,col = c("red","green","blue","orange"),names.arg  = c("Accuracy","sensitivity","pecificity"),xlab = "LYMPH",ylab="performance measures")    
    }})
}
shinyApp(shinyUI,shinyserver)


