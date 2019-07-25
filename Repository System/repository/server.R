library(shiny)
library(plotrix)
library(jsonlite)
#path<-file.path("repository.csv")
#path2<-file.path("Sales.csv")
sales<-read.csv("Sales.csv",stringsAsFactors = FALSE)

repo<-read.csv("repository.csv",stringsAsFactors = FALSE)

  function(input,output,session){
 
    observeEvent(input$updateddata,{
      qlbl<-renderText({input$quan1})
      plbl<-renderText({input$prod1})
      prlbl<-renderText({input$price1})
      slbl<-renderText({input$size})
      
     session$sendCustomMessage(type="testmessage",message="Data updated")
     p=c(prlbl())
     pp=as.numeric(p)
     q=c(qlbl())
     qq=as.numeric(q)
     s=pp*qq
     data1<-c(plbl(),slbl(),qlbl(),prlbl(),s)
     
     write.table(rbind(data1),repo,row.names = FALSE,col.names = FALSE,sep = ",",append = TRUE)
      
    })

    data2<-reactive({
      c(input$prod1,input$quan1,input$price1,input$size)
    })
    
   # print(data2())
    
    selectedData <- reactive({

      repo[,c(repo$size, repo$totalprice)]
    })
    selectedData2 <- reactive({
      sales<-read.csv("Sales.csv",stringsAsFactors = FALSE)
      
      c(sales$Totalprofits, sales$Totalsales)
    })
    
    value<-reactive({
      input$value
    })
    predictproduct<-reactive({
      input$predictproduct
    })
    
    
    clusters <- reactive({
    kmeans(c(repo$quantity,repo$totalprice),input$clusters)
          })
    clusters2 <- reactive({
      kmeans(selectedData2(),input$cltrs)
    })
    
    
    output$clust <- renderPlot({
      palette(c("red", "black", "blue"))
      repo<-read.csv("repository.csv",stringsAsFactors = FALSE)
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(c(repo$quantity,repo$totalprice),
           col = clusters()$cluster,
           pch = 20, cex = 3,xlab="quantity",ylab="price")
      #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
     # legend(x="bottomright",legend=c('Price','quantity'))
      
      
    })
    output$clust2 <- renderPlot({
      palette(c("black", "darkblue", "red"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData2(),
           col = clusters2()$cluster,
           pch = 20, cex = 3,xlab="",ylab="Profits")
     # points(clusters2()$centers, pch = 4, cex = 4, lwd = 4)
      #legend(x="bottomright",legend=c('Price','quantity'))
      
      
    })
    output$graph<-renderPlot({
      jan<-c(sales$sales.jan)
      feb<-c(sales$sales.feb)
      mar<-c(sales$sales.mar)
      colors=c("black","white","blue")
      products<-sales$productname
      month<-c("January","February","March")
      values<-matrix(c(jan,feb,mar),nrow=3,ncol = 3,byrow = FALSE)
      barplot(values,names.arg=month,col=colors,xlab="Month",ylab = "sales",main = "SALES GRAPH")
      legend("topleft",products,cex=1,fill = colors)
      
 
      
    })
    
    
    output$graph2<-renderPlot({
      jan<-c(sales$profits.jan)
      feb<-c(sales$profits.feb)
      mar<-c(sales$profits.mar)
      colors=c("black","white","blue")
      products<-sales$productname
      month<-c("January","February","March")
      values<-matrix(c(jan,feb,mar),nrow=3,ncol = 3,byrow = FALSE)
      barplot(values,names.arg=month,col=colors,xlab="Month",ylab = "Profits",main = "PROFITS GRAPH")
      legend("topleft",products,cex=1,fill = colors)
      
      
      
    })
    
    output$bar2 <- renderPlot({
      repo<-read.csv("repository.csv",stringsAsFactors = FALSE)
      quantity<-repo$quantity
      product<-repo$product
     barplot(quantity,names.arg =product ,xlab = "Products",ylab="Quantity",col="black",main = "PRODUCTS QUANTITY CHART",border="white")
      
    })
    
    output$bar1 <- renderPlot({
      repo<-read.csv("repository.csv",stringsAsFactors = FALSE)
      product<-repo$product
      
      price<-repo$totalprice
      barplot(price,names.arg =product ,xlab = "Products",ylab="Price",col="black",main = "PRODUCTS PRICES CHART",border="white")
      
      
    })
 
    output$pie2 <- renderPlot({
      sales<-read.csv("Sales.csv",stringsAsFactors = FALSE)
      
      x<-sales$Totalprofits
      lbl<-c(sales$productname)
      piepercent<-round(100*x/sum(x),1)
      print(x)
      pie(x, piepercent,radius = 1, main = "PRofits Pie Chart",col = rainbow(length(x)))
      legend("topright",lbl,cex = 0.8,fill = rainbow(length(x)))
      
    })
    
    
  output$table <- DT::renderDataTable(DT::datatable({
    repo<-read.csv("repository.csv",stringsAsFactors = FALSE)
    
    data <- repo
    if (input$quantity != "All") {
      data <- data[data$quantity == input$quantity,]
      
    }
    if (input$totalprice != "All") {
      data <- data[data$totalprice  == input$totalprice,]
      
    }
    if (input$product!= "All") {
      data <- data[data$product == input$product,]
      
    }
    data
  }))
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    repo<-read.csv("repository.csv",stringsAsFactors = FALSE)
    
    data <- repo

    data
  }))
  output$table3 <- DT::renderDataTable(DT::datatable({
    repo<-read.csv("repository.csv",stringsAsFactors = FALSE)
    
    data <- repo
    
    data
  }))
  
  output$prof <- DT::renderDataTable(DT::datatable({
    sales<-read.csv("Sales.csv",stringsAsFactors = FALSE)
    
    data <- sales
    
    data
  }))
  
output$plot1<-renderPlot({
  #NUTS::
  jan<-c(sales$sales.jan[1])
  feb<-c(sales$sales.feb[1])
  mar<-c(sales$sales.mar[1])
  nuts<-c(jan,feb,mar)
  janp<-c(sales$profits.jan[1])
  febp<-c(sales$profits.feb[1])
  marp<-c(sales$profits.mar[1])
  nutsp<-c(janp,febp,marp)
  #BOLTS::
  janb<-c(sales$sales.jan[2])
  febb<-c(sales$sales.feb[2])
  marb<-c(sales$sales.mar[2])
  bolts<-c(janb,febb,marb)
  janbp<-c(sales$profits.jan[2])
  febbp<-c(sales$profits.feb[2])
  marbp<-c(sales$profits.mar[2])
  boltsp<-c(janbp,febbp,marbp)
  #STEELBARS::
  jans<-c(sales$sales.jan[3])
  febs<-c(sales$sales.feb[3])
  mars<-c(sales$sales.mar[3])
  steels<-c(jans,febs,mars)
  jansp<-c(sales$profits.jan[3])
  febsp<-c(sales$profits.feb[3])
  marsp<-c(sales$profits.mar[3])
  steelsp<-c(jansp,febsp,marsp)
  
  
  print("NUTS:")
  print(nuts)
  print(nutsp)
  
  #totalsales<-sales$Totalsales
  #totalprofits<-sales$Totalprofits
 # m<-c(totalprofits,totalsales)
#  m1<-c(nutsp,nuts)
 # m2<-c(boltsp,bolts)
  #m3<-c(steelsp,steels)
  #model<-lm(totalprofits~totalsales)
  
  model1<-lm(nutsp~nuts)
  model2<-lm(boltsp~bolts)
  model3<-lm(steelsp~steels)
  
  if (input$predictproduct == "Nuts") {
    plot(nutsp,nuts,col="black",main="NUTS PROFITS and SALES REGRESSION",
         abline(lm(nuts~nutsp)),cex=1.3,pch=16,xlab="PROFITS",ylab="SALES")
    
    a<-data.frame(nuts=value())
    result<-predict(model1,a)
    
  }
  if(input$predictproduct=="Bolts"){
    
    plot(boltsp,bolts,col="black",main="BOLTS PROFITS and SALES REGRESSION",
         abline(lm(bolts~boltsp)),cex=1.3,pch=16,xlab="PROFITS",ylab="SALES")
    
    
    b<-data.frame(bolts=value())
    result<-predict(model2,b)
    
    
  }
  if(input$predictproduct=="steelbars"){
    
    
    plot(steelsp,steels,col="black",main="STEELs PROFITS and SALES REGRESSION",
         abline(lm(steels~steelsp)),cex=1.3,pch=16,xlab="PROFITS",ylab="SALES")
    
    
    c<-data.frame(steels=value())
    result<-predict(model3,c)
    
    
  }
  
  
  
  
  
    print("PREDICTION::")
  print(result)
  print(predictproduct())
  output$predictionvalue<-renderText(result)
  
 # output$pred1p<-renderText(anova(model1)$'Pr(>F)'[1])
})
#totalsales=as.double(sales$Totalsales)
#totalprofits=as.double(sales$Totalprofits)
#relation<-lm(totalprofits~totalsales)
#output$pred1p<-renderText(anova(relation)$'Pr(>F)'[1])
#output$pred1slope<-renderText(anova(model1[[1]][2]))
#output$pred1intercept<-renderText(anova(model1[[1]][1]))

}
