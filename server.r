library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(shinyTime)
library(data.table)
library(shinyjs)
library(revgeo)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

  # leafletOutput("matg")

server <- function(input, output, session) {
  
  # date14<- Sys.Date()- years(4)
  
  output$sdi <- renderPrint({ input$date })
  output$sti <- renderPrint({ input$time })
  output$msdi <- renderUI({ 
    fluidRow(
      column(width=12,height=7,
             dateInput("date", label = h3("Date"), value = Sys.Date()),
             timeInput("time", label = h3("Time"), value = Sys.time()),
             br(),
             actionButton('locater', "Set location")))
    
    })
  
  observeEvent(input$locater, {
    newtab<-switch(input$tabs, "sd"="sl")
    updateTabItems(session, "tabs", newtab)
  })
  
  # atg<-reactive()
  mymap = createLeafletMap(session, 'mymap')
  
  session$onFlushed(once = T, function() {
    
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = -73.9927, lat = 40.733, zoom = 16) %>%
 
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }")))  
  })
  })
  
  
  clic<-NULL
  
  makeReactiveBinding("clic")
  observe({
    clic<<-input$mymap_click
    if(is.null(clic))
      return()
    
    useShinyjs()
    shinyjs::show(id="haha")
    text<-paste("Latitude ", clic$lat, "Longitude ", clic$lng)
    # text2<-paste("You've selected point ", click$text)
    
    leafletProxy(mapId = "mymap") %>%
      clearPopups() %>%
      addPopups(dat = clic, lat = ~lat, lng = ~lng, popup = text)
    
    
    # newtab<-switch(input$tabs, "sl"="lrc")
    # updateTabItems(session, "tabs", newtab)
    })
  
  
  
  output$mapi<- renderUI({
    tabsetPanel(tabPanel("Enter the location you would receive recommnedations for. Use the Locate me button to pick a location close to you",
                         leafletOutput("mymap"), fluidRow(verbatimTextOutput("Clic_text")))
                         ) 
  })
  
  
  numDays <- function(date) {
    m <- format(date, format="%m")
    
    while (format(date, format="%m") == m) {
      date <- as.Date(date) + 1
    }
    
    return(as.integer(format(date - 1, format="%d")))
  }
  time<-reactive({input$time})
  #time<-Sys.time()
  
  time1<-reactive({as.ITime(time() + 10*60, tz="")})
  
  time2<-reactive({as.ITime(time() - 10*60, tz="")})
  
  # time1<-reactive({as.ITime(time1(), tz="")})
  # 
  # time2<-reactive({as.ITime(time2(), tz="")})
  
  date<-reactive({as.Date(input$date)})
  
  date1<-reactive({as.Date(date())-7})
  
  date4<-reactive({as.Date(date())-1})
  
  date2<-reactive({as.Date(date())-30})
  
  date3<-reactive({date()-numDays(date())})
  
  day1<-reactive({strftime(date(),'%u')})
  
  atg<-reactive({select(filter(uber14,timex>time2() & timex<time1()),Lat, Lon,Date,day) })
  
  # atg<-reactive({select(filter(uber14, strptime(uber14$Time, "%H:%M") < strptime(time1(), "%H:%M") & strptime(uber14$Time, "%H:%M") > strptime(time2(), "%H:%M")),Lat, Lon,Date,day)})
  
  # atg <- reactive({subset(uber14, timex > time2() | timex < time1(),
  #        select=c(Lat, Lon,Date,day))}) 
  
  yd<-reactive({filter(atg(), as.Date(atg()[["Date"]], "%Y-%M-%d")==date4())})
  
  l7d<-reactive({filter(atg(), as.Date(atg()[["Date"]], "%Y-%M-%d")<date() & as.Date(atg()[["Date"]],"%Y-%M-%d")>date1())})
  
  l1m<-reactive({filter(atg(), as.Date(atg()[["Date"]], "%Y-%M-%d")<date() & as.Date(atg()[["Date"]],"%Y-%M-%d")>date2())})
  
  lw<-reactive({filter(atg(), as.Date(atg()[["Date"]], "%Y-%M-%d")==date1())})
  
  epw<-reactive({filter(atg(), day==day1())})
  
  lm<-reactive({filter(atg(), as.Date(atg()[["Date"]], "%Y-%M-%d")==date3())})
  
  alat<-NULL
  blat<-NULL
  alon<-NULL
  blon<-NULL

  makeReactiveBinding("alat")
  makeReactiveBinding("blat")
  makeReactiveBinding("alon")
  makeReactiveBinding("blon")
  
  observe({
  alat<<-clic$lat-0.004
  blat<<-clic$lat+0.004
  
  alon<<-clic$lng-0.004
  blon<<-clic$lng+0.004
  })
  
  lrd<-reactive({select(filter(atg(), atg()[["Lat"]]>alat & atg()[["Lat"]]<blat & atg()[["Lon"]]>alon & atg()[["Lon"]]<blon), Lat, Lon) })
  
  # dt<-reactive({as.data.frame(lrd())})
  LATgrid<-reactive({cut(lrd()[["Lat"]],seq(min(lrd()[["Lat"]]),max(lrd()[["Lat"]]),length.out=5),include.lowest = TRUE)})
  LONgrid<-reactive({cut(lrd()[["Lon"]],seq(min(lrd()[["Lon"]]),max(lrd()[["Lon"]]),length.out=5),include.lowest = TRUE)})
  
  IDgrid<-reactive({interaction(LATgrid(),LONgrid())})
  
  IDNgrid<-reactive({factor(IDgrid())})
  
  ddata<-reactive({data.table(Lat=lrd()$Lat,Lon=lrd()$Lon,Latg=LATgrid(),Long=LONgrid(),Fact=IDNgrid(),p="")})
  
  counter<-reactive({aggregate(ddata()$Lat,ddata()[,c('Latg','Long','Fact')],FUN=length)})
  # newd <-  reactive({ddata() %>% group_by(Fact) %>% filter(n()>1)})

  # dricord<-reactive({data.table(drilat=point()$V1,drilon=point()$V2)})
  
  
  # cou<-reactive({aggregate(drilat(),ddata()[,c('Latg','Long','Fact')],FUN=length)})
  drilat<-reactive({runif(25, alat, blat)})
  drilon<-reactive({runif(25, alon, blon)}) 
  point<-reactive({cbind(drilat(),drilon())})
  coux<-reactive({cut(drilat(),seq(min(lrd()[["Lat"]]),max(lrd()[["Lat"]]),length.out=5),include.lowest = TRUE)})
  cous<-reactive({cut(drilon(),seq(min(lrd()[["Lon"]]),max(lrd()[["Lon"]]),length.out=5),include.lowest = TRUE)})
  couz<-reactive({interaction(coux(),cous())})
  fcouz<-reactive({factor(couz())})
  ffcouz<-reactive({data.table(drlat=drilat(),drlon=drilon(),Latgd=coux(),Longd=cous(),Fact=fcouz())})
  coun<-reactive({aggregate(ffcouz()$drlat,ffcouz()[,c('Latgd','Longd','Fact')],FUN=length)})
  pvalue<-reactive({counter()[["x"]]/(1+coun()[["x"]])})
  qvalue<-reactive({counter()[["x"]]})
  # counter()$p <- reactive({counter %>%
  final<-reactive({data.table(p = ifelse(counter()$Fact %in% coun()$Fact, pvalue(), qvalue()),Fact=counter()$Fact)})
  
  
  # for(counter()[["Fact"]] %in% coun()[["Fact"]])
  # {
  #   counter()$p<-reactive({counter()[["x"]]/(1+coun()[["x"]])})
  # }
  # for(counter()[["Fact"]] %in% coun()[["Fact"]])
  # {
  #   counter()$p<-reactive({counter()[["x"]]})
  # }
  
  # output$mlrd4<- renderTable ({
  #   coun()
  # })
  output$mlrd5<- renderTable ({
    final()
  })
  
  highes<-reactive({max(final()[["p"]], na.rm=TRUE)})
  highest<-reactive({filter(final(),final()$p==highes())})
  
  # atg<-reactive({select(filter(uber14,timex>time2() & timex<time1()),Lat, Lon,Date,day) })
  
  gridd<-reactive({filter(counter(), counter()$Fact==highest()$Fact)})
  
  griddd<-reactive({filter(ddata(), ddata()$Latg==gridd()$Latg & ddata()$Long==gridd()$Long)})
  
  lrlat<-reactive({mean(griddd()$Lat)})
  lrlon<-reactive({mean(griddd()$Lon)})
  
  output$mlrd<-renderLeaflet({
    leaflet() %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addCircleMarkers(lng = lrlon(),lat = lrlat(), radius = 40) 
  })
  
  # addrlr<-revgeo(lrlon(), lrlat())
  
  textlrd<-reactive({paste("Head over to", 
                           revgeo(lrlon(), lrlat(), provider='google', API='AIzaSyAS1bLmOH-iUmqs82nKEbBxHckQaQNS_9w'),
                           "as indicated by marker in the map above")})
  
  output$mlrd2<- renderText ({
    textlrd()
  })
  
  
  # x <- reactive({seq(round(alat,3),round(blat,3),by=0.002)})
  # y <- reactive({seq(round(alon,3),round(blon,3),by=0.002)})
  # 
  # 
  # gri <- reactive({expand.grid(x(), y())})
  # N <- 13

  output$mlrd0 <- renderPlot({
    # plot(gri(), t="n", xaxs="i", yaxs="i")
    smoothScatter(lrd(), colramp = colorRampPalette(c("white", blues9)),ret.selection = TRUE, nrpoints = 1000)
    # points(point(),col="red", pch="+")
    # plot(lrd()[["Lat"]], lrd()[["Lon"]], col="blue", pch ="+")
    
    
    # abline(v=x(), h=y(),lty=2)
      })
  
  # binxy <- reactive({data.frame(x()=findInterval(points[,1], x()),
  #                     y()=findInterval(points[,2], y()))})
  # 
  # results <- table(binxy())
  
  
  # threshold.in.km<-0.003
  # dist.in.km.matrix <- reactive({rdist.earth(lrd(), miles = F, R=6371)})
  # 
  # fit<-reactive({hclust(as.dist(dist.in.km.matrix()),method="single")})
  # clusters<-reactive({cutree(fit(), h =threshold.in.km)})
  
  
  observe({output$mlrd1 <-renderLeaflet({
    leaflet(data=lrd()) %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addMarkers(~Lon, ~Lat, clusterOptions = markerClusterOptions()) 
  })
  
  # output$mlrd0 <- renderPlot({
  #   ggplot2::ggplot(data = lrd(), aes(x = lrd()[["Lat"]], y = lrd()[["Lon"]],z=NULL)) + stat_summary2d(fun=sum)
  # })
  
  # output$mlrd <- renderPlot({
  #   plot(lrd()[["Lat"]], lrd()[["Lon"]], col="blue", pch ="+", grid(col = "darkgrey"))
  # })
  })
  output$mlrdi<- renderUI({
    tabsetPanel(
      tabPanel("Distance train",fluidRow(leafletOutput("mlrd"),
                                                            textOutput("mlrd2"))),
      tabPanel("Test plot",plotOutput("mlrd0")),
      # tabPanel("Table",tableOutput("mlrd4")),
      tabPanel("Table",tableOutput("mlrd5")),
      tabPanel("Test map", leafletOutput("mlrd1"))
      
    
      # tabPanel("Plots1",plotOutput("mlrd")),

      )
  })
  
  clat<-NULL
  dlat<-NULL
  clon<-NULL
  dlon<-NULL
  
  makeReactiveBinding("clat")
  makeReactiveBinding("dlat")
  makeReactiveBinding("clon")
  makeReactiveBinding("dlon")
  
  observe({
    clat<<-clic$lat-0.005
    dlat<<-clic$lat+0.005
    
    clon<<-clic$lng-0.005
    dlon<<-clic$lng+0.005
  })
  
  lrp<-reactive({select(filter(atg(), atg()[["Lat"]]>clat & atg()[["Lat"]]<dlat & atg()[["Lon"]]>clon & atg()[["Lon"]]<dlon), Lat, Lon) })
  
  # dt<-reactive({as.data.frame(lrd())})
  LATgridd<-reactive({cut(lrp()[["Lat"]],seq(min(lrp()[["Lat"]]),max(lrp()[["Lat"]]),length.out=5),include.lowest = TRUE)})
  LONgridd<-reactive({cut(lrp()[["Lon"]],seq(min(lrp()[["Lon"]]),max(lrp()[["Lon"]]),length.out=5),include.lowest = TRUE)})
  
  IDgridd<-reactive({interaction(LATgridd(),LONgridd())})
  
  IDNgridd<-reactive({factor(IDgridd())})
  
  ddatax<-reactive({data.table(Lat=lrp()$Lat,Lon=lrp()$Lon,Latg=LATgridd(),Long=LONgridd(),Fact=IDNgridd())})
  
  counterx<-reactive({aggregate(ddatax()$Lat,ddatax()[,c('Latg','Long','Fact')],FUN=length)})
  # newd <-  reactive({ddata() %>% group_by(Fact) %>% filter(n()>1)})
  # count<- reactive({ave(ddata()$Lat,ddata()$Fact,FUN=length);})
  
  drilatt<-reactive({runif(25, clat, dlat)})
  drilonn<-reactive({runif(25, clon, dlon)}) 
  points<-reactive({cbind(drilatt(),drilonn())})
  couxs<-reactive({cut(drilatt(),seq(min(lrp()[["Lat"]]),max(lrp()[["Lat"]]),length.out=5),include.lowest = TRUE)})
  couss<-reactive({cut(drilonn(),seq(min(lrp()[["Lon"]]),max(lrp()[["Lon"]]),length.out=5),include.lowest = TRUE)})
  couzs<-reactive({interaction(couxs(),couss())})
  fcouzs<-reactive({factor(couzs())})
  ffcouzs<-reactive({data.table(drlat=drilatt(),drlon=drilonn(),Latgd=couxs(),Longd=couss(),Fact=fcouzs())})
  couns<-reactive({aggregate(ffcouzs()$drlat,ffcouzs()[,c('Latgd','Longd','Fact')],FUN=length)})
  pvalues<-reactive({counterx()[["x"]]/(1+couns()[["x"]])})
  qvalues<-reactive({counterx()[["x"]]})
  # counter()$p <- reactive({counter %>%
  finals<-reactive({data.table(p = ifelse(counterx()$Fact %in% couns()$Fact, pvalues(), qvalues()),Fact=counterx()$Fact)})
  
  
  highess<-reactive({max(finals()[["p"]], na.rm=TRUE)})
  highests<-reactive({filter(finals(),finals()$p==highess())})
  
  # atg<-reactive({select(filter(uber14,timex>time2() & timex<time1()),Lat, Lon,Date,day) })
  
  griddx<-reactive({filter(counterx(), counterx()$Fact==highests()$Fact)})
  
  gridddx<-reactive({filter(ddatax(), ddatax()$Latg==griddx()$Latg & ddatax()$Long==griddx()$Long)})
  
  lrlatx<-reactive({mean(gridddx()$Lat)})
  lrlonx<-reactive({mean(gridddx()$Lon)})
  
  output$mlrp<-renderLeaflet({
    leaflet() %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addCircleMarkers(lng = lrlonx(),lat = lrlatx(), radius = 40) 
  })
  
  # addrlr<-revgeo(lrlon(), lrlat())
  
  textlrdx<-reactive({paste("Head over to", 
                            revgeo(lrlonx(), lrlatx(), provider='google', API='AIzaSyAS1bLmOH-iUmqs82nKEbBxHckQaQNS_9w'),
                            "as indicated by marker in the map above")})
  
  output$mlrp3<- renderTable ({
    finals()
  })
  
  output$mlrp2<- renderText ({
    textlrdx()
  })
  
  # x1 <- reactive({seq(round(clat,3),round(dlat,3),by=0.003)})
  # y1 <- reactive({seq(round(clon,3),round(dlon,3),by=0.003)})
  # 
  # 
  # grix <- reactive({expand.grid(x1(), y1())})
  # N <- reactive({nrow(lrp())})
  
  # point <- reactive({cbind(runif(N(), alat, blat), runif(N(), alon, blon))})
  
  output$mlrp0<- renderPlot({
    # plot(grix(), t="n", xaxs="i", yaxs="i")
    # plot(lrd()[["Lat"]], lrd()[["Lon"]], col="blue", pch ="+")
    smoothScatter(lrp(), colramp = colorRampPalette(c("white", blues9)),ret.selection = TRUE, nrpoints = 1000)
    # points(lrp(), col="blue", pch="+")
    # abline(v=x1(), h=y1(),lty=2)
  })
  
  # binxy <- reactive({data.frame(x()=findInterval(points[,1], x()),
  #                     y()=findInterval(points[,2], y()))})
  # 
  # results <- table(binxy())
  
  
  # threshold.in.km<-0.003
  # dist.in.km.matrix <- reactive({rdist.earth(lrd(), miles = F, R=6371)})
  # 
  # fit<-reactive({hclust(as.dist(dist.in.km.matrix()),method="single")})
  # clusters<-reactive({cutree(fit(), h =threshold.in.km)})
  
  
  observe({output$mlrp1 <-renderLeaflet({
    leaflet(data=lrp()) %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addMarkers(~Lon, ~Lat, clusterOptions = markerClusterOptions()) 
  })
  
  # output$mlrd0 <- renderPlot({
  #   ggplot2::ggplot(data = lrd(), aes(x = lrd()[["Lat"]], y = lrd()[["Lon"]],z=NULL)) + stat_summary2d(fun=sum)
  # })
  
  # output$mlrd <- renderPlot({
  #   plot(lrd()[["Lat"]], lrd()[["Lon"]], col="blue", pch ="+", grid(col = "darkgrey"))
  # })
  })
  output$mlrpi<- renderUI({
    tabsetPanel(
      tabPanel("Probability train",fluidRow(leafletOutput("mlrp"),
                                                            textOutput("mlrp2")
                                                            )),
      tabPanel("Test plot",plotOutput("mlrp0")),
      tabPanel("Table",tableOutput("mlrp3")),
      tabPanel("Test map", leafletOutput("mlrp1"))
      # tabPanel("Plots1",plotOutput("mlrd")),

    )
  })

  elat<-NULL
  flat<-NULL
  elon<-NULL
  flon<-NULL
  
  makeReactiveBinding("elat")
  makeReactiveBinding("flat")
  makeReactiveBinding("elon")
  makeReactiveBinding("flon")
  
  observe({
    elat<<-clic$lat-0.006
    flat<<-clic$lat+0.006
    
    elon<<-clic$lng-0.006
    flon<<-clic$lng+0.006
  })
  
  lrc<-reactive({select(filter(epw(), epw()[["Lat"]]>elat & epw()[["Lat"]]<flat & epw()[["Lon"]]>elon & epw()[["Lon"]]<flon), Lat, Lon) })
  
  # dt<-reactive({as.data.frame(lrd())})
  LATgri<-reactive({cut(lrc()[["Lat"]],seq(min(lrc()[["Lat"]]),max(lrc()[["Lat"]]),length.out=5),include.lowest = TRUE)})
  LONgri<-reactive({cut(lrc()[["Lon"]],seq(min(lrc()[["Lon"]]),max(lrc()[["Lon"]]),length.out=5),include.lowest = TRUE)})

  IDgri<-reactive({interaction(LATgri(),LONgri())})
  
  IDNgri<-reactive({factor(IDgri())})
  
  ddataxx<-reactive({data.table(Lat=lrc()$Lat,Lon=lrc()$Lon,Latg=LATgri(),Long=LONgri(),Fact=IDNgri())})
  
  counterxx<-reactive({aggregate(ddataxx()$Lat,ddataxx()[,c('Latg','Long','Fact')],FUN=length)})
  # newd <-  reactive({ddata() %>% group_by(Fact) %>% filter(n()>1)})
  # count<- reactive({ave(ddata()$Lat,ddata()$Fact,FUN=length);})
  drilattt<-reactive({runif(25, elat, flat)})
  drilonnn<-reactive({runif(25, elon, flon)}) 
  pointss<-reactive({cbind(drilattt(),drilonnn())})
  couxss<-reactive({cut(drilattt(),seq(min(lrc()[["Lat"]]),max(lrc()[["Lat"]]),length.out=5),include.lowest = TRUE)})
  cousss<-reactive({cut(drilonnn(),seq(min(lrc()[["Lon"]]),max(lrc()[["Lon"]]),length.out=5),include.lowest = TRUE)})
  couzss<-reactive({interaction(couxss(),cousss())})
  fcouzss<-reactive({factor(couzss())})
  ffcouzss<-reactive({data.table(drlat=drilattt(),drlon=drilonnn(),Latgd=couxss(),Longd=cousss(),Fact=fcouzss())})
  counss<-reactive({aggregate(ffcouzss()$drlat,ffcouzss()[,c('Latgd','Longd','Fact')],FUN=length)})
  pvaluess<-reactive({counterxx()[["x"]]/(1+counss()[["x"]])})
  qvaluess<-reactive({counterxx()[["x"]]})
  # counter()$p <- reactive({counter %>%
  finalss<-reactive({data.table(p = ifelse(counterxx()$Fact %in% counss()$Fact, pvaluess(), qvaluess()),Fact=counterxx()$Fact)})
  
  
  highesss<-reactive({max(finalss()[["p"]], na.rm=TRUE)})
  highestss<-reactive({filter(finalss(),finalss()$p==highesss())})
  
  # atg<-reactive({select(filter(uber14,timex>time2() & timex<time1()),Lat, Lon,Date,day) })
  
  griddxx<-reactive({filter(counterxx(), counterxx()$Fact==highestss()$Fact)})
  
  gridddxx<-reactive({filter(ddataxx(), ddataxx()$Latg==griddxx()$Latg & ddataxx()$Long==griddxx()$Long)})
  
  lrlatxx<-reactive({mean(gridddxx()$Lat)})
  lrlonxx<-reactive({mean(gridddxx()$Lon)})
  output$mlrc3<- renderTable ({
    finalss()
  })
  
  output$mlrc<-renderLeaflet({
    leaflet() %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addCircleMarkers(lng = lrlonxx(),lat = lrlatxx(), radius = 40) 
  })
  
  # addrlr<-revgeo(lrlon(), lrlat())
  
  textlrdxx<-reactive({paste("Head over to", 
                             revgeo(lrlonxx(), lrlatxx(), provider='google', API='AIzaSyAS1bLmOH-iUmqs82nKEbBxHckQaQNS_9w'),
                             "as indicated by marker in the map above")})
  
  output$mlrc2<- renderText ({
    textlrdxx()
  })
  
  # x2 <- reactive({seq(round(elat,3),round(flat,3),by=0.003)})
  # y2 <- reactive({seq(round(elon,3),round(flon,3),by=0.003)})
  # 
  # 
  # grixx <- reactive({expand.grid(x2(), y2())})
  # N <- reactive({nrow(lrp())})
  
  # point <- reactive({cbind(runif(N(), alat, blat), runif(N(), alon, blon))})
  
  output$mlrc0<- renderPlot({
    # plot(grixx(), t="n", xaxs="i", yaxs="i")
    # plot(lrd()[["Lat"]], lrd()[["Lon"]], col="blue", pch ="+")
    smoothScatter(lrc(), colramp = colorRampPalette(c("white", blues9)),ret.selection = TRUE, nrpoints = 1000)
    # points(lrc(), col="blue", pch="+")
    # abline(v=x2(), h=y2(),lty=2)
  })
  
  # binxy <- reactive({data.frame(x()=findInterval(points[,1], x()),
  #                     y()=findInterval(points[,2], y()))})
  # 
  # results <- table(binxy())
  
  
  # threshold.in.km<-0.003
  # dist.in.km.matrix <- reactive({rdist.earth(lrd(), miles = F, R=6371)})
  # 
  # fit<-reactive({hclust(as.dist(dist.in.km.matrix()),method="single")})
  # clusters<-reactive({cutree(fit(), h =threshold.in.km)})
  
  
  observe({output$mlrc1 <-renderLeaflet({
    leaflet(data=lrc()) %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addMarkers(~Lon, ~Lat, clusterOptions = markerClusterOptions()) 
  })
  
  # output$mlrd0 <- renderPlot({
  #   ggplot2::ggplot(data = lrd(), aes(x = lrd()[["Lat"]], y = lrd()[["Lon"]],z=NULL)) + stat_summary2d(fun=sum)
  # })
  
  # output$mlrd <- renderPlot({
  #   plot(lrd()[["Lat"]], lrd()[["Lon"]], col="blue", pch ="+", grid(col = "darkgrey"))
  # })
  })
  output$mlrci<- renderUI({
    tabsetPanel(
      tabPanel("Consistency train",fluidRow(leafletOutput("mlrc"),
                                                            textOutput("mlrc2"))),
      tabPanel("Test Plot",plotOutput("mlrc0")),
      tabPanel("Table",tableOutput("mlrc3")),
      tabPanel("Test map", leafletOutput("mlrc1"))
      # tabPanel("Plots1",plotOutput("mlrd")),
      
    )
  })
  
  output$matg <- renderLeaflet({
    leaflet(data=atg()) %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addMarkers(~Lon, ~Lat, clusterOptions = markerClusterOptions())
  })

  output$matgi<- renderUI({
tabsetPanel(tabPanel("General Pickups at Current Time",leafletOutput("matg")))
  })

  output$myd <- renderLeaflet({
    leaflet(data=yd()) %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addMarkers(~Lon, ~Lat, clusterOptions = markerClusterOptions())
  })

  output$mydi<- renderUI({
    tabsetPanel(tabPanel("Pickups Yesterday at Current Time",leafletOutput("myd")))
  })

  output$ml7d <- renderLeaflet({
    leaflet(data=l7d()) %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addMarkers(~Lon, ~Lat, clusterOptions = markerClusterOptions())
  })

  output$ml7di<- renderUI({
    tabsetPanel(tabPanel("Pickups in the last 7 Days at Current Time",leafletOutput("ml7d")))
  })

  output$ml1m <- renderLeaflet({
    leaflet(data=l1m()) %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addMarkers(~Lon, ~Lat, clusterOptions = markerClusterOptions())
  })

  output$ml1mi<- renderUI({
    tabsetPanel(tabPanel("Pickups in the last 30 days at Current Time",leafletOutput("ml1m")))
  })

  output$mlw <- renderLeaflet({
    leaflet(data=lw()) %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addMarkers(~Lon, ~Lat, clusterOptions = markerClusterOptions())
  })

  output$mlwi<- renderUI({
    tabsetPanel(tabPanel("Pickups Last week at Current Time",leafletOutput("mlw")))
  })

  output$mlm <- renderLeaflet({
    leaflet(data=lm()) %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addMarkers(~Lon, ~Lat, clusterOptions = markerClusterOptions())
  })

  output$mlmi<- renderUI({
    tabsetPanel(tabPanel("Pickups Last month at Current Time",leafletOutput("mlm")))
  })

  output$mepw <- renderLeaflet({
    leaflet(data=epw()) %>% setView(lng = clic$lng, lat = clic$lat, zoom = 14) %>% addTiles()%>% addMarkers(~Lon, ~Lat, clusterOptions = markerClusterOptions())
  })

  output$mepwi<- renderUI({
    tabsetPanel(tabPanel("Pickups at Current Day and Time in the Past",leafletOutput("mepw")))
  })

}

