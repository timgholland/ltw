library(shiny)
source("FVSOpCostShiny.R")
library(ggplot2)
options(scipen = 999)


shinyServer(function(input, output) {
    
  options(scipen=300)
  
  output$title1<-reactive({
    inFile<-input$file1
    
    if(is.null(inFile))
      return("")
    
    "Analyst Assigned Operations: Future Cost"
  })
  
  
  contentsTable<-reactive({
    inFile<-input$file1
    
    if(is.null(inFile))
      return(NULL)
    
    m<-read.csv(file=inFile$datapath, header=TRUE, sep=",", colClasses=c("Stand"="character"))
    
    m2<-prescribe(m,if(input$customChoice==TRUE){
      customInputs2()} else if(input$state=="Idaho"){
        idahoCost} else if(input$state=="Oregon"){
          oregonCost} else if(input$state=="Washington"){
            washingtonCost} else {NULL})
    
    m3<-if(input$customChoice==TRUE){
      customInputs2()} else if(input$state=="Idaho"){
      idahoCost} else if(input$state=="Oregon"){
        oregonCost} else if(input$state=="Washington"){
          washingtonCost} else {NULL}
    
    m4<-ifelse((m$"RxPackage_Rx_RxCycle")==30501, yarderEst(m,m3), 
               ifelse((m$"RxPackage_Rx_RxCycle")==30400, harSkidEst(m,m3),
                      ifelse((m$"RxPackage_Rx_RxCycle")==30300, fbSkidEst(m,m3),
                             ifelse((m$"RxPackage_Rx_RxCycle")==30500, harForEst(m,m3), 1))))
    
    
    
    m6<-data.frame(dput(m$"Stand"), m2, labelEq1(m))
    
    colnames(m6)<-c("Stand ID", "Treatment Cost (Per Acre)", "Treatment Selection")
    
    m6
  })
  
  idealTable<-reactive({
    iChoice<-input$idealChoice
    
    if(iChoice==FALSE)
      return(NULL)
    
    File1<-input$file1
    
    n<-read.csv(file=File1$datapath, header=TRUE, sep=",", colClasses=c("Stand"="character"))
    
    n2<-slopeEq(n)
    
    n3<-if(input$customChoice==TRUE){
      customInputs2()} else if(input$state=="Idaho"){
      idahoCost} else if(input$state=="Oregon"){
        oregonCost} else if(input$state=="Washington"){
          washingtonCost} else (1)
    
    n4<-ifelse(n$"Percent.Slope">45.001, yarderEst(n,n3), ifelse(
           harForEst(n,n3) & harSkidEst(n,n3)>fbSkidEst(n,n3), fbSkidEst(n,n3), ifelse(
             fbSkidEst(n,n3) & harForEst(n,n3)>harSkidEst(n,n3), harSkidEst(n,n3), ifelse(
               harSkidEst(n,n3) & fbSkidEst(n,n3)>harForEst(n,n3), harForEst(n,n3), NULL))))
    
    
    n6<-data.frame(n$"Stand",labelEq2(n,n3), labelEq3(n,n3))
    
    colnames(n6)<-c("Stand ID", "Treatment Cost (Per Acre)", "Treatment Selection")
    
    n6
  })
  
  output$contents<-renderTable({
   
   contentsTable()
   
  })
  
  output$title2<-reactive({
    pChoice<-input$plotChoice
    
    if(pChoice==FALSE)
      return("")
    
    "Plot of the Future Cost of Analysts Assigned Treatments"
  })
  
  output$costPlot<-renderPlot({
    
    pChoice<-input$plotChoice
    
    if(pChoice==FALSE)
      return(NULL)
        
    m6<<-contentsTable()
    
    p<-ggplot(data = m6, aes(x = as.character(m6$"Stand ID"), y = m6["Treatment Cost (Per Acre)"], label=m5["Treatment Selection"]))+
      geom_text(aes(label=m6["Treatment Selection"]),hjust=0, vjust=0)+
      xlab("Stand ID")+
      ylab("Treatment Cost ($/Acre)")+theme(axis.text.x=element_text(angle=-30, hjust=-.05))
    
    print(p)
    
  })
  
  output$title3<-reactive({
    iChoice<-input$idealChoice
    
    if(iChoice==FALSE)
      return("")
    
    "Simulated Cheapest Operations: Future Cost"
  })
  
  output$ideal<-renderTable({
    
    idealTable()
    
    })  
  
  downloadSelection<-reactive({
    switch(input$downloadSelect,
           "Prescribed Future Cost Table"=contentsTable(),
           "Simulated Cheapest Future Cost Table"=idealTable(),
           "Prescribed Present Cost Table"=presentCost(),
           "Simulated Cheapest Present Cost Table"=presentIdealCost())
  })
  
  output$downloadData<- downloadHandler(
    filename=function(){
      paste(input$downloadSelect, '.csv', sep="")
    },
    content=function(file){
       write.csv(downloadSelection(), file)
             }
  )
  
  output$title4<-reactive({
    pChoice<-input$presentChoice
    
    if(pChoice==FALSE)
      return("")
    
    "Analyst Selected Forest Operations: Present Cost"
  })
  
  presentCost<-reactive({
    
    pChoice<-input$presentChoice
    
    if(pChoice==FALSE)
      return(NULL)
    
    
    inFile<-input$file1
    
    m<-read.csv(file=inFile$datapath, header=TRUE, sep=",", colClasses=c("Stand"="character"))
    
    m5<-prescribe(m,if(input$customChoice==TRUE){
      customInputs2()} else if(input$state=="Idaho"){
        idahoCost} else if(input$state=="Oregon"){
          oregonCost} else if(input$state=="Washington"){
            washingtonCost} else {NULL})
    
    
    m6<-(m5/(1+input$interest)^((as.numeric(m[,"YearCostCalc"]))))
    
    m7<-data.frame(m$"Stand", m6, labelEq1(m))
    
    colnames(m7)<-c("Stand ID", "Treatment Cost (Per Acre)", "Treatment Selection")
    
    m7
  })
  
  output$presentTable<-renderTable({
    
    presentCost()
    
  })
  
  output$title5<-reactive({
    
    pIChoice<-input$presentIdealChoice
    
    if(pIChoice==FALSE)
      return("")
    
    "Calculated Cheapest Operations: Present Cost"
    
  })
  
  presentIdealCost<-reactive({
    
    pIChoice<-input$presentIdealChoice
    
    if(pIChoice==FALSE)
      return(NULL)
    
    File1<-input$file1
    
    n<-read.csv(file=File1$datapath, header=TRUE, sep=",", colClasses=c("Stand"="character"))
    
    n2<-slopeEq(n)
  
    n3<-if(input$customChoice==TRUE){
      customInputs2()} else if(input$state=="Idaho"){
      idahoCost} else if(input$state=="Oregon"){
        oregonCost} else if(input$state=="Washington"){
          washingtonCost} else (1)
    
    n4<-ifelse(n$"Percent.Slope">45.001, yarderEst(n,n3), ifelse(
      harForEst(n,n3) & harSkidEst(n,n3)>fbSkidEst(n,n3), fbSkidEst(n,n3), ifelse(
        fbSkidEst(n,n3) & harForEst(n,n3)>harSkidEst(n,n3), harSkidEst(n,n3), ifelse(
          harSkidEst(n,n3) & fbSkidEst(n,n3)>harForEst(n,n3), harForEst(n,n3), NULL))))
    
    
    n6<-(n4/(1+input$interest)^((as.numeric(n[,"YearCostCalc"]))))
    
    n7<-data.frame(n$"Stand", n6, labelEq2(n,n3))
    
    colnames(n7)<-c("Stand ID", "Treatment Cost (Per Acre)", "Treatment Selection")
    
    n7
    
    
  })
  
  output$presentIdealTable<-renderTable({
    
    presentIdealCost()
    
  })
  
  output$title6<-reactive({
    
    xChoice<-input$presentPlotChoice
    
    if(xChoice==FALSE)
      return("")
    
    "Calculated Cheapest Operations: Present Cost"
        
  })
  
  output$presentPlot<-renderPlot({
    
    pPChoice<-input$presentPlotChoice
    
    if(pPChoice==FALSE)
      return(NULL)
    
    m5<<-presentCost()
    
    p<-ggplot(data = m5, aes(x = as.character(m5$"Stand ID"), y = m5["Treatment Cost (Per Acre)"], label=m5["Treatment Selection"]))+
      geom_text(aes(label=m5["Treatment Selection"]),hjust=0, vjust=0)+
      xlab("Stand ID")+
      ylab("Treatment Cost ($/Acre)")+theme(axis.text.x=element_text(angle=-30, hjust=-.05))
    
    print(p)
    
  })
  
  output$title7<-reactive({
    
    yChoice<-input$idealPlotChoice
    
    if(yChoice==FALSE)
      return("")
    
    "Calculated Cheapest Operations: Future Cost"
    
  })
  
  output$idealPlot<-renderPlot({
    
    yPChoice<-input$idealPlotChoice
    
    if(yPChoice==FALSE)
      return(NULL)
    
    m5<<-idealTable()
    
    p<-ggplot(data = m5, aes(x = as.character(m5$"Stand ID"), y = m5["Treatment Cost (Per Acre)"], label=m5["Treatment Selection"]))+
      geom_text(aes(label=m5["Treatment Selection"]),hjust=0, vjust=0)+
      xlab("Stand ID")+
      ylab("Treatment Cost ($/Acre)")+theme(axis.text.x=element_text(angle=-30, hjust=-.05))
    
    print(p)
    
  })
  
  output$title8<-reactive({
    
    qChoice<-input$presentIdealPlotChoice
    
    if(qChoice==FALSE)
      return("")
    
    "Calculated Cheapest Operations: Present Cost"
    
  })
  
  output$presentIdealPlot<-renderPlot({
    
    fPChoice<-input$presentIdealPlotChoice
    
    if(fPChoice==FALSE)
      return(NULL)
    
    m5<<-presentIdealCost()
    
    p<-ggplot(data = m5, aes(x = as.character(m5$"Stand ID"), y = m5["Treatment Cost (Per Acre)"], label=m5["Treatment Selection"]))+
      geom_text(aes(label=m5["Treatment Selection"]),hjust=0, vjust=0)+
      xlab("Stand ID")+
      ylab("Treatment Cost ($/Acre)")+theme(axis.text.x=element_text(angle=-30, hjust=-.05))
    
    print(p)
    
  })
  
  output$title9<-reactive({
    
    fCChoice<-input$futureComboChoice
    
    if(fCChoice==FALSE)
      return(NULL)
    
    "Prescribed and Cheapest Operations: Future Cost"
    
  })
  
  output$futureComboPlot<-renderPlot({
    
    fCChoice<-input$futureComboChoice
    
    if(fCChoice==FALSE)
      return(NULL)
    
    DF1<-contentsTable()
    
    DF2<-idealTable()
    
    DF11<<-data.frame(DF1$"Stand ID", DF1$"Year", DF1$"Treatment Cost", DF2$"Treatment Cost (Per Acre)", DF1$"Treatment Selection")
            
    colnames(DF11)<-c("Stand ID", "Year", "Analyst Assigned Treatment Cost", "Ideal Treatment Cost", "FVS OpCost Treatment Selection")
    
    DF11<<-DF11
    
    p<-ggplot(DF11, aes(x=as.character(DF11$"Stand ID"), y=DF11$"Analyst Assigned Treatment Cost", color="Simulated Treatment", shape="Year"))+
      geom_point(aes(y=DF11$"Analyst Assigned Treatment Cost", shape=as.character(DF11$"Year"), color="Prescribed"), size=4)+
      geom_point(aes(y=DF11$"Ideal Treatment Cost", shape=as.character(DF11$"Year"), color="Cheapest"), size=4)+      
      xlab("Stand ID")+
      ylab("Cost")+theme(axis.text.x=element_text(angle=-30, hjust=-.05))
    
    print(p)
    
  })
  
  customInputs<-reactive({
    
    xyChoice<-input$customChoice
    
    if(xyChoice==FALSE)
      return(NULL)
    
    data.frame(
      row.names = c("Feller Buncher", 
                    "Forwarder",
                    "Harvester",
                    "Grapple Skidder",
                    "Cable Skidder",
                    "Wheeled Feller Buncher",
                    "Crawler Feller Buncher",
                    "Yarder",
                    "Slideboom",
                    "Manual Feller",
                    "Chipper"),
      Value = c(input$fellerBuncher, 
                             input$forwarder,
                             input$harvester,
                             input$grappleSkidder,
                             input$cableSkidder,
                             input$wheelFeller,
                             input$crawlerFeller,
                             input$yarder,
                             input$slideboom,    
                             input$chainsaw,
                             input$chipper), 
      stringsAsFactors=FALSE)
  }) 
  
  customInputs2<-reactive({
    
    ci<-customInputs()
    
    row.names(ci)<-c("fellerBuncher","forwarder","harvester","grappleSkidderLarge","cableSkidderLarge","wheelFellerBuncher","crawlerFellerBuncher","yarder","slideboom","chainsaw","chipper")
    
    ci
  })
  
  output$values <- renderTable({
    
    customInputs()
  })
  
})
   
       


 
