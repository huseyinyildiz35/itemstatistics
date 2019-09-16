library(psych)
library(shiny)
library(readxl)
library(shinythemes)
library(xlsxjars)
library(xlsx)
ui <- fluidPage(theme = shinytheme("darkly"),
  # Application title
  titlePanel("ITEM STATISTICS APPLICATION",br()), 
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Choose File", placeholder="File",buttonLabel = "Add",
                accept = c(
                  "xlsx",
                  "xls",
                  "csv")
      ),hr(),"Follow these instructions;",br(), "-Your data must be in excel format",br(), "-Don't use Turkish characters in file name",br(),"-Don't use any column or row names",br(),"-Your data must be dichotomous (1-0)",
      hr(),"Developed by Huseyin YILDIZ",br(),"e-mail: huseyinyildiz35@gmail.com"),
    # Show a plot of the generated distribution
    mainPanel(
              tabsetPanel(
      tabPanel("Difficulty Index",br(),"Interpretations reference, Crocker&Algina (2008).",br(),"Click button for download table",br(),downloadButton('downloaddifficulty','Download Table'),hr(),tableOutput("difficulty")),
      tabPanel("Discrimination Index",br(),"Interpretations reference , Crocker&Algina (2008).",br(),"Click button for download table",br(),downloadButton('downloaddiscrimination','Download Table'),hr(),tableOutput("discrimination")),
      tabPanel("Item Variances",br(),"Click button for download table",br(),downloadButton('downloaddeviance','Download Table'),hr(),tableOutput("deviance")),
      tabPanel("Item Reliability",br(),"Click button for download table",br(),downloadButton('downloadreliability','Download Table'),hr(),tableOutput("reliability")),
      tabPanel("Distribution",plotOutput("distribution"))
    )
     
    )
  )
)
server <- shinyServer(function(input, output) {
  ##Madde Gucluk indeksi kodlari
  
  diffbutton<-reactive({
    inFile <- input$dataset
    if (is.null(inFile))
      return("Please upload data")
    dataset<- read_xlsx(inFile$datapath, sheet=1)
    data<-as.matrix(dataset)
    for(i in 1:nrow(data)){
      for(j in 1:ncol(data)){
        if(data[i,j]==1 | data[i,j]==0){
          a<-1
        }
        else {a<-0}
      }} 
    t<-"Unsuitable Data Type, Please Use Binary Data"
    if(a!=1){
      return(t)
    }
    p<-c()
    for(i in 1:ncol(data)){
      p[i]<-round(sum(data[,i])/nrow(data),3)
    }
    item<-c()
    for(i in 1:ncol(data)){
      item[i]<-paste(i,".item")
    }
    Interpretion<-c()
    for(i in 1:ncol(data)){
      if(p[i]<0.30){
        Interpretion[i]<-"Hard"
      }
      if(0.30<p[i] & p[i]<0.60){
        Interpretion[i]<-"Medium"
      }
      if(p[i]>0.60){
        Interpretion[i]<-"Easy"
      }
    }
    difftable<-cbind(item,p,Interpretion)
    difftable<-as.data.frame(difftable)
    difftable
    
  })
  discbutton<-reactive({
    inFile <- input$dataset
    if (is.null(inFile))
      return("Please upload data")
    dataset<- read_xlsx(inFile$datapath, sheet=1)
    data<-as.matrix(dataset)
    for(i in 1:nrow(data)){
      for(j in 1:ncol(data)){
        if(data[i,j]==1 | data[i,j]==0){
          a<-1
        }
        else{a<-0}
      }
    } 
    t<-"Unsuitable Data Type, Please Use Binary Data"
    if(a!=1){
      return(t)
    }
    
    totaltest<-c()
    for(i in 1:nrow(data)){
      totaltest[i]<- sum(data[i,])/ncol(data)*100
    }
    rpbis<-c()
    for(i in 1:ncol(data)){
      rpbis[i]<-round(cor(totaltest,data[,i]),3)
    }
    scores<-c()
    uppersum<-c()
    lowersum<-c()
    for(i in 1:nrow(data)){
      scores[i]<-sum(data[i,]/ncol(data)*100)
    }
    for(i in 1:ncol(data)){
      q<-cbind(data,scores)
      a<-q[order(q[,ncol(q)],decreasing =TRUE),]
      n<-round(nrow(a)*27/100,0)
      uppersum[i]<-sum(a[1:n,i])
    }
    for(i in 1:ncol(data)){
      q<-cbind(data,scores)
      a<-q[order(q[,ncol(q)],decreasing =FALSE),]
      n<-round(nrow(a)*27/100,0)
      lowersum[i]<-sum(a[1:n,i])
    }
    UL27<-round((uppersum-lowersum)/n,3)
    item<-c()
    for(i in 1:ncol(data)){
      item[i]<-paste(i,".item")
    }
    Interpretion<-c()
    for(i in 1:ncol(data)){
      if(rpbis[i]<0.20){
        Interpretion[i]<-"Low discrimination (Eliminate Item)"
      }
      if(0.20<=rpbis[i] & rpbis[i]<0.30){
        Interpretion[i]<-"Medium Discrimination (Revise Item)"
      }
      if(rpbis[i]>=0.30){
        Interpretion[i]<-"High discrimination (Good Item)"
      }
    }
    tlast<-cbind(item,rpbis,UL27,Interpretion)
    tlast
  })
  deviancebutton<-reactive({
    inFile <- input$dataset
    item1  <- input$item
    item2  <-as.numeric(item1)
    if (is.null(inFile))
      return("Please upload data")
    dataset<- read_xlsx(inFile$datapath, sheet=1)
    data<-as.matrix(dataset)
    for(i in 1:nrow(data)){
      for(j in 1:ncol(data)){
        if(data[i,j]==1 | data[i,j]==0){
          a<-1
        }
        else{a<-0}
      }
    } 
    t<-"Unsuitable Data Type, Please Use Binary Data"
    if(a!=1){
      return(t)
    }
    
    p<-c()
    for(i in 1:ncol(data)){
      p[i]<-round(sum(data[,i])/nrow(data),3)
    }
    variance<-c()
    for(i in 1:ncol(data)){
      variance[i]<-round(p[i]*(1-p[i]),3)
    }
    s.deviation<-c()
    for(i in 1:ncol(data)){
      s.deviation[i]<-round(sqrt(variance),3)
    }
    item<-c()
    for(i in 1:ncol(data)){
      item[i]<-paste(i,".item")
    }
    deviances<-cbind(item,s.deviation,variance)
    deviances
  })
  relibutton<-reactive({
    inFile <- input$dataset
    if (is.null(inFile))
      return("Please upload data")
    dataset<- read_xlsx(inFile$datapath, sheet=1)
    data<-as.matrix(dataset)
    for(i in 1:nrow(data)){
      for(j in 1:ncol(data)){
        if(data[i,j]==1 | data[i,j]==0){
          a<-1
        }else{a<-0}
      }
    } 
    t<-"Unsuitable Data Type, Please Use Binary Data"
    if(a!=1){
      return(t)
    }
    
    
    p<-c()
    for(i in 1:ncol(data)){
      p[i]<-round(sum(data[,i])/nrow(data),3)
    }
    variance<-c()
    for(i in 1:ncol(data)){
      variance[i]<-round(p[i]*(1-p[i]),3)
    }
    s.deviation<-c()
    for(i in 1:ncol(data)){
     s.deviation[i]<-round(sqrt(variance),3)
    }
    item<-c()
    for(i in 1:ncol(data)){
      item[i]<-paste(i,".item")
    }
    testscore<-c()
    for(i in 1:nrow(data)){
      testscore[i]<- sum(data[i,])/ncol(data)*100
    }
    rpbis<-c()
    for(i in 1:ncol(data)){
      rpbis[i]<-round(cor(testscore,data[,i]),3)
    }
    reliability<-c()
    for(i in 1:ncol(data)){
      reliability[i]<- round(s.deviation[i]*rpbis[i],3)
    }
    relitable<-cbind(item,reliability)
  })
  distbutton<-reactive({
    inFile <- input$dataset
    dataset<- read_xlsx(inFile$datapath, sheet=1)
    data<-as.matrix(dataset)
    scores<-c()
    for(i in 1:nrow(data)){
      scores[i]<-sum(data[i,])/ncol(data)*100
    }
   hist(scores,col = "gray",xlab = "Total test scores",ylab = "Frequency",main = "Total test score frequency distribution",border = "black",plot = TRUE,freq = TRUE,breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))
  })
  output$difficulty <- renderTable({
    # generate bins based on input$bins from ui.R
    diffbutton()
  })
  ##Madde Ayiricilik indeksi kodlari
  output$discrimination<-renderTable({
    discbutton()
  })
  ##Standart Sapma kodlari
  output$deviance<-renderTable({
    deviancebutton()
  })
  output$reliability<-renderTable({
    relibutton()
  })
  output$distribution<-renderPlot({
    distbutton()
  })
  
 
  output$downloaddifficulty <- downloadHandler(
    filename = function() {
      paste("diffindexes", ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(diffbutton(), file,row.names = FALSE)
    }
  )
  output$downloaddiscrimination <- downloadHandler(
    filename = function() {
      paste("discindexes",".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(discbutton(), file,row.names = FALSE)
    }
  )
  output$downloaddeviance <- downloadHandler(
    filename = function() {
      paste("itemdeviances", ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(deviancebutton(), file,row.names = FALSE)
    }
  )
  output$downloadreliability <- downloadHandler(
    filename = function() {
      paste("reliablity", ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(relibutton(), file,row.names = FALSE)
    }
  )

})
# Run the application 
shinyApp(ui, server)
