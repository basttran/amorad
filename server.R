####Libraries####
library(shiny)
library(shinyjs)
library(leaflet)
# INSTALL VERSION FROM RSTUDIO'S GITHUB 'devtools::install_github("rstudio/leaflet")'
library(plyr)
library(dplyr)
library(shinyAce)
library(sp)
library(DT)
library(DataCombine)
#######Server data######
server <- reactiveValues(collection = matrix(list(),nrow=0,ncol=5),
                         corpus = matrix(list(),nrow=0,ncol=4),
                         cloud = matrix(list(),nrow=0,ncol=3),
                         web = NULL,
                         chat = NULL,
                         users = NULL)

####Server data from files####
if (file.exists("collection")){
  server$collection <- readRDS("collection")
}
if (file.exists("corpus")){
  server$corpus <- readRDS("corpus")
}
if (file.exists("cloud")){
  server$cloud <- readRDS("cloud")
}
if (file.exists("chat")){
  server$chat <- readRDS("chat")
}
####Functions####
#Get the line prefix
linePrefix <- function(){
  if (is.null(isolate(server$chat))){
    return("")
  }
  return("<br /><br />")
}


re.id <- function(x) {
  x[[3]]@ID<-x[[1]]
  return(x)
}
node.collector <- function(x) {
  x<-x$nodes
  return(x)
}

coord.id <- function(x) {
  x<- c(x[[3]]@coords[1],x[[3]]@coords[2],x[[1]])
  return(x)
}
spots.data <- function(x) {
  x<-matrix(x,nrow=1)
  coord.x1<-x[,3][[1]]@coords[1]
  coord.x2<-x[,3][[1]]@coords[2]
  id<-x[,1]
  x<-c(coord.x1,coord.x2,id)
  return(x)
}
add.tag <- function(x) {
  x$tags<-rbind(x$tags,data.frame(user = 'user', tag = 'tag', stringsAsFactors = FALSE))
  return(x)
}
#######Client data######

shinyServer(function(input, output, session) {
  
  client <- reactiveValues(entity = NULL,
                           note = NULL,
                           buffer = NULL,
                           prototype = NULL,
                           user = NULL,
                           chatlog = NULL)
####Client data from files####
  if (file.exists("chatlog.Rds")){
    client$chatlog <- readRDS("chatlog.Rds")
  }
  
####Chat####
  sessionVars <- reactiveValues(username = "")
  # Track whether or not this session has been initialized. We'll use this to
  # assign a username to unininitialized sessions.
  init <- FALSE
  # When a session is ended, remove the user and note that they left the room.
  session$onSessionEnded(function() {
    isolate({
      server$users <- server$users[server$users != client$user]
      server$chat <- c(server$chat, paste0(linePrefix(),
                                           tags$span(class="user-exit",
                                                     client$user,
                                                     "left the room.")))
    })
  })
####List chat users####
  #Observer to handle changes to the username
  observe({
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user
    if (!init){
      # Seed initial username
      if (length(server$users) != 0) {
        client$user <- "user2"
      } else {
        client$user <- "user1"
      }
      isolate({
        server$chat <<- c(server$chat, paste0(linePrefix(), tags$span(class="user-enter", client$user, "entered the room.")))
      })
      init <<- TRUE
    } else{
      return()
    }
    # Add this user to the global list of users
    isolate(server$users <- c(server$users, client$user))
  })
  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul(lapply(server$users, function(user){
      return(tags$li(user))
    })))
  })
####Send new entry####  
  observe({
    if(input$send < 1){
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    isolate({
      # Add the current entry to the chat log.
      inCollection<-data.frame(from=unlist(server$collection[,1]), to = paste0('<a class="collection" href="#" data-label="',unlist(server$collection[,1]),'">',unlist(server$collection[,1]),'</a>'))
      inCorpus<-data.frame(from=unlist(server$corpus[,1]), to = paste0('<a class="corpus" href="#" data-label="',unlist(server$corpus[,1]),'">',unlist(server$corpus[,1]),'</a>'))
      inCloud<-data.frame(from=unlist(server$cloud[,1]), to = paste0('<a class="cloud" href="#" data-label="',unlist(server$cloud[,1]),'">',unlist(server$cloud[,1]),'</a>'))
      inline<-rbind(inCollection,inCorpus,inCloud)
      inline<-mutate(inline, length=nchar(as.character(from)))
      inline<-mutate(inline, length=nchar(as.character(from)))
      inline<-inline[order(-inline$length), ]
      inline<-inline[,1:2]
      client$chatlog<<-rbind(client$chatlog,data.frame(time=Sys.time(),user = client$user, txt=input$entry, stringsAsFactors = FALSE))
      saveRDS(client$chatlog,"chatlog")
      echo <-data.frame(txt=unlist(strsplit(input$entry,split=" ")),stringsAsFactors = FALSE)
      saveRDS(log,"log")
      server$chat <- c(server$chat, 
                       paste0(linePrefix(), tags$span(class="username", tags$abbr(title=tail(client$chatlog$time,1), tail(client$chatlog$user,1))),
                              ": ",
                              paste(FindReplace(echo, 
                                          Var = "txt",
                                          replaceData = inline, 
                                          from = "from", 
                                          to = "to", exact = TRUE, vector = TRUE), sep="", collapse=" ")))
    })
    # Clear out the text entry field.
    updateTextInput(session, "entry", value="")
  })
  
####Chat window####
  # Dynamically create the UI for the chat window.
  output$chat <- renderUI({
    if (length(server$chat) > 500){
      # Too long, use only the most recent 500 lines
      server$chat <- server$chat[(length(server$chat)-500):(length(server$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(server$chat, "chat")
    # Pass the chat log through as HTML

    HTML(server$chat)
  })

  # observe(server$chatlog, {
  #   
  # })  
  
####Display UserId####  
  output$user <- renderUI({
    HTML(paste("Your ID is"," '",client$user,"'."))
  })

####Display conversational tags####
  observe({
    if (is.null(input$showEntity))
      return()
    isolate({
      if (which(server$collection[,1]==input$showEntity$label) %in% input$collection_rows_selected) {
        proxycol <- dataTableProxy('collection')
        proxycol %>% selectRows(input$collection_rows_selected[input$collection_rows_selected!=which(server$collection[,1]==input$showEntity$label)])
      } else {
        proxycol <- dataTableProxy('collection')
        proxycol %>% selectRows(c(input$collection_rows_selected,which(server$collection[,1]==input$showEntity$label)))  
      }
    })
  })
  observe({
    # print("lol")
    if (is.null(input$showNote))
      return()
    isolate({
      if (which(server$corpus[,1]==input$showNote$label) %in% input$corpus_rows_selected) {
        proxycor <- dataTableProxy('corpus')
        proxycor %>% selectRows(input$corpus_rows_selected[input$corpus_rows_selected!=which(server$corpus[,1]==input$showNote$label)])
      } else {
        proxycor <- dataTableProxy('corpus')
        proxycor %>% selectRows(c(input$corpus_rows_selected,which(server$corpus[,1]==input$showNote$label)))  
      }
    })
  })
  observe({
    if (is.null(input$showTag))
      return()
    isolate({
      if (which(server$cloud[,1]==input$showTag$label) %in% input$cloud_rows_selected) {
        proxyclo <- dataTableProxy('cloud')
        proxyclo %>% selectRows(input$cloud_rows_selected[input$cloud_rows_selected!=which(server$cloud[,1]==input$showTag$label)])
      } else {
        proxyclo <- dataTableProxy('cloud')
        proxyclo %>% selectRows(c(input$cloud_rows_selected,which(server$cloud[,1]==input$showTag$label)))  
      }
    })
  })

####Getting click events######
  observeEvent(input$map_click,   {
    # map clicks are added to client$buffer and a new item in is created server$nodes
    if (input$entityType == 0) {return()}
    event <- data.frame(lng=input$map_click$lng, lat=input$map_click$lat, layerId=as.character(length(server$nodes)+1),stringsAsFactors = FALSE) 
    leafletProxy("map") %>% removeMarker(layerId = client$buffer$layerId)
    client$buffer <- rbind(client$buffer,event)
    server$nodes[[length(server$nodes)+1]] <- event
  })
  
  observeEvent(input$map_shape_click,   {
    # clicked shapes are added to client$buffer and a new item in is created server$nodes
    if (input$entityType == 0) {return()}
    event <- data.frame(lng=input$map_shape_click$lng, lat=input$map_shape_click$lat, layerId=as.character(length(server$nodes)+1),stringsAsFactors = FALSE) 
    leafletProxy("map") %>% removeMarker(layerId = client$buffer$layerId)
    client$buffer <- rbind(client$buffer,event)
    server$nodes[[length(server$nodes)+1]] <- event
  })
  
  observeEvent(input$map_marker_click,   {
    # clicked markers are added to client$buffer
    if (input$entityType == 0) {return()}
    event <- data.frame(lng=input$map_marker_click$lng, lat=input$map_marker_click$lat, layerId=as.character(input$map_marker_click$id), stringsAsFactors = FALSE)
    client$buffer <- rbind(client$buffer,event)
  })
####Drawing shapes####
  observeEvent(client$buffer, {
    # we remove temporary shapes
    leafletProxy("map") %>% removeShape(layerId= "proto")
    leafletProxy("map") %>% removeMarker("proto")
    leafletProxy("map") %>% removeMarker(layerId=client$buffer$layerId)
    if (input$entityType == 0) {
      # if no type is selected nothing happens
      return()
    } else if (input$entityType == "point") {
      # adding a point
      if (tail(client$buffer$layerId,1)=="proto" && length(client$buffer$layerId) > 1 ) {
        client$prototype <-SpatialPoints(cbind(as.vector(client$buffer[2,1]),as.vector(client$buffer[2,2])))
        leafletProxy("map") %>% addCircleMarkers(data=client$prototype,layerId="entityNew", color = "green")
        # client$entity<-c(label = paste0(client$user,"@",input$entityLabel), type = input$entityType, nodes = client$prototype, user = client$user, date = Sys.Date())
        client$buffer<-NULL
      } else {
        client$buffer<-tail(client$buffer,1)
        client$prototype <-SpatialPoints(cbind(as.vector(client$buffer[1,1]),as.vector(client$buffer[1,2])))
        leafletProxy("map") %>% addCircleMarkers(data=client$prototype,layerId="proto", color = "blue")
      }
    } else if (input$entityType == "line") {
      # adding a line
      if (head(client$buffer$layerId,1) == tail(client$buffer$layerId,1) && length(client$buffer$layerId) > 1 ) {
        client$prototype <-Lines(list(Line(cbind(as.vector(client$buffer[-nrow(client$buffer),1]),as.vector(client$buffer[-nrow(client$buffer),2])))), "proto")
        leafletProxy("map") %>% addPolylines(data=client$prototype,layerId="entityNew", color = "green")
        # client$prototype@ID<-paste0(client$user,"@",input$entityLabel)
        # client$entity<-c(label = paste0(client$user,"@",input$entityLabel), type = input$entityType, nodes = client$prototype, user = client$user, date = Sys.Date())
        client$buffer<-NULL
      } else {
        client$prototype <-Lines(list(Line(cbind(as.vector(client$buffer[,1]),as.vector(client$buffer[,2])))), "proto")
        leafletProxy("map") %>% addPolylines(data=client$prototype,layerId="proto", color = "blue")
        leafletProxy("map") %>% addCircleMarkers(lng = client$buffer$lng, lat = client$buffer$lat, layerId = unique(as.character(client$buffer$layerId)), color = "grey")    # server$elements[[event$Id]] <- event
      }
    } else if (input$entityType == "polygon") {
      # adding a polygon
      client$prototype <- Polygons(list(Polygon(as.matrix(rbind(client$buffer[,1:2],client$buffer[1,1:2])))), "proto")
      if ( head(client$buffer$layerId,1) == tail(client$buffer$layerId,1) && length(client$buffer$layerId) > 1 ) {
        leafletProxy("map") %>% addPolygons(data=client$prototype,layerId="entityNew", color = "green")
        # client$prototype@ID<-paste0(client$user,"@",input$entityLabel)
        # client$entity<-c(label = paste0(client$user,"@",input$entityLabel), type = input$entityType, nodes = client$prototype, user = client$user, date = Sys.Date())
        client$buffer<-NULL
      } else {
        leafletProxy("map") %>% addPolygons(data=client$prototype,layerId="proto", color = "blue")
        leafletProxy("map") %>% addCircleMarkers(lng = client$buffer$lng, lat = client$buffer$lat, layerId = client$buffer$layerId, color = "grey")    # server$elements[[event$Id]] <- event
      }
    }
  })
  
####Entity creation####  
  observeEvent(input$entityNew, {
    if (input$entityType == 'line' || input$entityType == 'polygon') {
      client$prototype@ID<-paste0(client$user,"@",input$entityLabel)
    }
    client$entity<-c(label = paste0(client$user,"@",input$entityLabel), type = input$entityType, nodes = client$prototype, user = client$user, date = Sys.Date())
    server$collection<-rbind(server$collection,client$entity)
    saveRDS(server$collection,"collection")
    # accès aux coord dans collection c[3,1][[1]][[1]]$lat
    client$entity<-NULL
    updateSelectInput(session,"entityType",selected= 0 )
    updateTextInput(session,"entityLabel",value = "")
    leafletProxy("map") %>% removeMarker(layerId = "entityNew" )
    leafletProxy("map") %>% removeShape(layerId = "entityNew" )
  })
####Note creation####
  observeEvent(input$noteNew, {
    server$corpus<-rbind(server$corpus,c(label = paste0(client$user,"§",input$noteLabel), body = input$noteBody, user = client$user, date = Sys.Date()))
    saveRDS(server$corpus,"corpus")
    updateTextInput(session,"noteLabel",value = "")
    updateAceEditor(session,"noteBody",value = 1)
  })
####Sample entities####
  observe({
    if (length(input$collection_rows_selected) == 0) {
      leafletProxy("map") %>% clearGroup("selected")
      updateAceEditor(session,"selectedEntity",value = 1)
    } else {
      leafletProxy("map") %>% clearGroup("selected")
      sample <- server$collection[input$collection_rows_selected,,drop=F]
      saveRDS(sample,"sample")
      updateAceEditor(session,"selectedEntity",value = paste(server$collection[input$collection_rows_selected,1],collapse = '\n'))
      spots <- sample[sample[,2,drop=F]=="point",,drop=F]
      if (length(spots) != 0) {
        spots <- matrix(unlist(apply(spots,1,spots.data)), ncol=3, byrow=T)
        leafletProxy("map") %>% addCircleMarkers(lng=as.numeric(spots[,1]), lat = as.numeric(spots[,2]), layerId = spots[,3], group = "selected")
      }
      lines <- sample[sample[,2,drop=F]=="line",,drop=F]
      if (length(lines) != 0) {
        leafletProxy("map") %>% addPolylines(data=SpatialLines(matrix(lines,ncol=5)[,3]), group="selected")
      }
      polys <- sample[sample[,2,drop=F]=="polygon",,drop=F]
      if (length(polys) != 0) {
        leafletProxy("map") %>% addPolygons(data=SpatialPolygons(matrix(polys,ncol=5)[,3]),group="selected")
      }
    }
  }
  )
####Sample note####
  observe({
    if (length(input$corpus_rows_selected) == 0) {
      updateAceEditor(session,"noteView",value = 1)
      updateAceEditor(session,"selectedNote",value = 1)
    } else {
      updateAceEditor(session,"noteView",value = server$corpus[tail(input$corpus_rows_selected,1),2])
      updateAceEditor(session,"selectedNote",value = paste(server$corpus[input$corpus_rows_selected,1],collapse = '\n'))
    }
  })
####Tag creation####

  output$tagEntity <- renderUI({
    aceEditor("tagEntityList", value= paste(server$collection[input$collection_rows_selected,1],collapse = '\n'), mode="jade", theme="ambient", readOnly = TRUE,
              height = "90px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
              hotkeys = list(nextInputKey = list(win="Tab",mac="Tab")))
  })
  output$tagNote <- renderUI({
    aceEditor("tagNoteList", value= paste(server$corpus[input$corpus_rows_selected,1],collapse = '\n'), mode="jade", theme="ambient", readOnly = TRUE,
              height = "90px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
              hotkeys = list(nextInputKey = list(win="Tab",mac="Tab")))
  })
  # output$tagTag <- renderUI({
  #   aceEditor("tagTagList", value= paste(server$cloud[input$cloud_rows_selected,1],collapse = '\n'), mode="jade", theme="ambient", readOnly = TRUE,
  #             height = "90px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
  #             hotkeys = list(nextInputKey = list(win="Tab",mac="Tab")))
  # })
  observeEvent(input$tagNew, {
    server$cloud<-rbind(server$cloud,c(label=paste0(client$user,"#",input$tagLabel), 
                                       entities = input$tagEntityList, 
                                       notes = input$tagNoteList))
    saveRDS(server$cloud,"cloud")
    updateTextInput(session,"tagLabel",value = "")
    proxycol <- dataTableProxy('collection')
    proxyclo <- dataTableProxy('cloud')
    selectRows(proxycol, NULL)
    selectRows(proxyclo, NULL)
  })
####Collection data table####
  output$collection <- DT::renderDataTable({
    DT::datatable(server$collection[,c(-3,-4,-5)],
                  options = list(pageLength = 5, 
                                 lengthChange=FALSE, 
                                 search = list(
                                   regex=TRUE)
                                 )
                  )
    })
####Corpus data table####
  output$corpus <- DT::renderDataTable({
    DT::datatable(data.frame(server$corpus[,c(-3,-4)]),
                  options = list(pageLength = 5, lengthChange=FALSE, search = list(regex=TRUE),
                                 columnDefs = list(
                                   list(
                                     targets = 2,
                                     render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 10 ?","'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;","}")
                                     )
                                   )
                                 )
                  # ,callback = JS('table.page(3).draw(false);')
                  )
    })
####Cloud data table####
  
  output$cloud <- DT::renderDataTable({
    DT::datatable(server$cloud,
                  options = list(pageLength = 5, lengthChange=FALSE, 
                                 search = list(regex=TRUE),
                                 columnDefs = list(
                                   list(
                                     className = "dt-center", 
                                     targets = 1,
                                     render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 0 ?","'<span title=\"' + data + '\">' + data.substr(0, 0) + '@</span>' : data;","}")
                                     ),
                                   list(
                                     className = "dt-center", 
                                     targets = 2,
                                     render = JS("function(data, type, row, meta) {","return type === 'display' && data.length > 0 ?","'<span title=\"' + data + '\">' + data.substr(0, 0) + '§</span>' : data;","}")
                                     )
                                   )
                                 )
                  # ,callback = JS('table.page(3).draw(false);')
                  )
    })
####Map management####
  output$map <- renderLeaflet({
   leaflet()  %>% 
      addTiles(urlTemplate="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = NULL,
               layerId = NULL,
               options = tileOptions()) %>% 
      setView(-1.685463,48.302370,6) 
    })
  observe({
    input$collection_rows_selected
    leafletProxy("map",session) %>% clearControls()
  })
  
  
  output$tag1 <- renderUI({
    selectInput("tag1","tag n°1",choices = as.list(as.data.frame(server$cloud)$label), selected = 0, multiple = FALSE, selectize = TRUE)
  })
  output$tag2 <- renderUI({
    selectInput("tag2","tag n°2",choices = as.list(as.data.frame(server$cloud)$label), selected = 0, multiple = FALSE, selectize = TRUE)
  })
  output$tag3 <- renderUI({
    selectInput("tag3","tag n°3",choices = as.list(as.data.frame(server$cloud)$label), selected = 0, multiple = FALSE, selectize = TRUE)
  })
  output$tag4 <- renderUI({
    selectInput("tag4","tag n°4",choices = as.list(as.data.frame(server$cloud)$label), selected = 0, multiple = FALSE, selectize = TRUE)
  })
  output$tag5 <- renderUI({
    selectInput("tag5","tag n°5",choices = as.list(as.data.frame(server$cloud)$label), selected = 0, multiple = FALSE, selectize = TRUE)
  })
  
  output$theme1 <- renderUI({
     aceEditor("content1", value= "", mode="text", theme="textmate", readOnly = TRUE,
               height = "450px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
               hotkeys = list(nextInputKey = list(win="Tab",mac="Tab")))
  })
  
  output$theme2 <- renderUI({
    aceEditor("content2", value= "", mode="text", theme="textmate", readOnly = TRUE,
              height = "450px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
              hotkeys = list(nextInputKey = list(win="Tab",mac="Tab")))
  })
  
  output$theme3 <- renderUI({
    aceEditor("content3", value= "", mode="text", theme="textmate", readOnly = TRUE,
              height = "450px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
              hotkeys = list(nextInputKey = list(win="Tab",mac="Tab")))
  })
  
  output$theme4 <- renderUI({
    aceEditor("content4", value= "", mode="text", theme="textmate", readOnly = TRUE,
              height = "450px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
              hotkeys = list(nextInputKey = list(win="Tab",mac="Tab")))
  })
  
  output$theme5 <- renderUI({
    aceEditor("content5", value= "", mode="text", theme="textmate", readOnly = TRUE,
              height = "450px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
              hotkeys = list(nextInputKey = list(win="Tab",mac="Tab")))
  })
  
  output$stamp1 <- renderUI({
    actionButton("stamp1",input$tag1, style = paste0('background-color: ',input$legend1,';'))
  })
  output$stamp2 <- renderUI({
    actionButton("stamp2",input$tag2, style = paste0('background-color: ',input$legend2,';'))
  })
  output$stamp3 <- renderUI({
    actionButton("stamp3",input$tag3, style = paste0('background-color: ',input$legend3,';'))
  })
  output$stamp4 <- renderUI({
    actionButton("stamp4",input$tag4, style = paste0('background-color: ',input$legend4,';'))
  })
  output$stamp5 <- renderUI({
    actionButton("stamp5",input$tag5, style = paste0('background-color: ',input$legend5,';'))
  })
  ###First tag####
  observe({
      if (is.null(input$tag1) || input$legend1 =="#FFFFFF") {return()}
      cloud<-as.data.frame(server$cloud)
      leafletProxy("map") %>% clearGroup("tag1")
      selected <- unlist(strsplit(unlist(cloud[which(cloud$label==input$tag1),2]),"\n"))
      mask<- unlist(server$collection[,1]) %in% selected
      saveRDS(input$legend1,"legend1")
      sample <- server$collection[mask,,drop=F]
      spots <- sample[sample[,2,drop=F]=="point",,drop=F]
      if (length(spots) != 0) {
        spots <- matrix(unlist(apply(spots,1,spots.data)), ncol=3, byrow=T)
        leafletProxy("map") %>% addCircleMarkers(lng=as.numeric(spots[,1]), lat = as.numeric(spots[,2]), layerId = spots[,3], group = "tag1",
                                                 stroke = TRUE, color = input$legend1, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$legend1,
                                                 fillOpacity = 0.2)
      }
      lines <- sample[sample[,2,drop=F]=="line",,drop=F]
      if (length(lines) != 0) {
        leafletProxy("map") %>% addPolylines(data=SpatialLines(matrix(lines,ncol=5)[,3]), group="tag1",
                                             stroke = TRUE, color = input$legend1, weight = 5, opacity = 0.5)
      }
      polys <- sample[sample[,2,drop=F]=="polygon",,drop=F]
      if (length(polys) != 0) {
        leafletProxy("map") %>% addPolygons(data=SpatialPolygons(matrix(polys,ncol=5)[,3]),group="tag1",
                                            stroke = TRUE, color = input$legend1, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$legend1,
                                            fillOpacity = 0.2)
      }
  }
  )
  
  ###Second tag####
  observe({
    if (is.null(input$tag2) || input$legend2 =="#FFFFFF") {return()}
    cloud<-as.data.frame(server$cloud)
    leafletProxy("map") %>% clearGroup("tag2")
    selected <- unlist(strsplit(unlist(cloud[which(cloud$label==input$tag2),2]),"\n"))
    mask<- unlist(collection[,1]) %in% selected
    saveRDS(input$legend2,"legend2")
    sample <- server$collection[mask,,drop=F]
    spots <- sample[sample[,2,drop=F]=="point",,drop=F]
    if (length(spots) != 0) {
      spots <- matrix(unlist(apply(spots,1,spots.data)), ncol=3, byrow=T)
      leafletProxy("map") %>% addCircleMarkers(lng=as.numeric(spots[,1]), lat = as.numeric(spots[,2]), layerId = spots[,3], group = "tag2",
                                               stroke = TRUE, color = input$legend2, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$legend2,
                                               fillOpacity = 0.2)
    }
    lines <- sample[sample[,2,drop=F]=="line",,drop=F]
    if (length(lines) != 0) {
      leafletProxy("map") %>% addPolylines(data=SpatialLines(matrix(lines,ncol=5)[,3]), group="tag2",
                                           stroke = TRUE, color = input$legend2, weight = 5, opacity = 0.5)
    }
    polys <- sample[sample[,2,drop=F]=="polygon",,drop=F]
    if (length(polys) != 0) {
      leafletProxy("map") %>% addPolygons(data=SpatialPolygons(matrix(polys,ncol=5)[,3]),group="tag2",
                                          stroke = TRUE, color = input$legend2, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$legend2,
                                          fillOpacity = 0.2)
    }
  }
  )
  
  ###Third tag####
  observe({
    if (is.null(input$tag3) || input$legend3 =="#FFFFFF") {return()}
    cloud<-as.data.frame(server$cloud)
    leafletProxy("map") %>% clearGroup("tag3")
    selected <- unlist(strsplit(unlist(cloud[which(cloud$label==input$tag3),2]),"\n"))
    mask<- unlist(collection[,1]) %in% selected
    saveRDS(input$legend3,"legend3")
    sample <- server$collection[mask,,drop=F]
    spots <- sample[sample[,2,drop=F]=="point",,drop=F]
    if (length(spots) != 0) {
      spots <- matrix(unlist(apply(spots,1,spots.data)), ncol=3, byrow=T)
      leafletProxy("map") %>% addCircleMarkers(lng=as.numeric(spots[,1]), lat = as.numeric(spots[,2]), layerId = spots[,3], group = "tag3",
                                               stroke = TRUE, color = input$legend3, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$legend3,
                                               fillOpacity = 0.2)
    }
    lines <- sample[sample[,2,drop=F]=="line",,drop=F]
    if (length(lines) != 0) {
      leafletProxy("map") %>% addPolylines(data=SpatialLines(matrix(lines,ncol=5)[,3]), group="tag3",
                                           stroke = TRUE, color = input$legend3, weight = 5, opacity = 0.5)
    }
    polys <- sample[sample[,2,drop=F]=="polygon",,drop=F]
    if (length(polys) != 0) {
      leafletProxy("map") %>% addPolygons(data=SpatialPolygons(matrix(polys,ncol=5)[,3]),group="tag3",
                                          stroke = TRUE, color = input$legend3, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$legend3,
                                          fillOpacity = 0.2)
    }
  }
  )
  
  ###Forth tag####
observe({
    if (is.null(input$tag4) || input$legend4 =="#FFFFFF") {return()}
    cloud<-as.data.frame(server$cloud)
    leafletProxy("map") %>% clearGroup("tag4")
    selected <- unlist(strsplit(unlist(cloud[which(cloud$label==input$tag4),2]),"\n"))
    mask<- unlist(collection[,1]) %in% selected
    saveRDS(input$legend4,"legend4")
    sample <- server$collection[mask,,drop=F]
    spots <- sample[sample[,2,drop=F]=="point",,drop=F]
    if (length(spots) != 0) {
      spots <- matrix(unlist(apply(spots,1,spots.data)), ncol=3, byrow=T)
      leafletProxy("map") %>% addCircleMarkers(lng=as.numeric(spots[,1]), lat = as.numeric(spots[,2]), layerId = spots[,3], group = "tag4",
                                               stroke = TRUE, color = input$legend4, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$legend4,
                                               fillOpacity = 0.2)
    }
    lines <- sample[sample[,2,drop=F]=="line",,drop=F]
    if (length(lines) != 0) {
      leafletProxy("map") %>% addPolylines(data=SpatialLines(matrix(lines,ncol=5)[,3]), group="tag4",
                                           stroke = TRUE, color = input$legend4, weight = 5, opacity = 0.5)
    }
    polys <- sample[sample[,2,drop=F]=="polygon",,drop=F]
    if (length(polys) != 0) {
      leafletProxy("map") %>% addPolygons(data=SpatialPolygons(matrix(polys,ncol=5)[,3]),group="tag4",
                                          stroke = TRUE, color = input$legend4, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$legend4,
                                          fillOpacity = 0.2)
    }
  }
  )
  
  ###Fifth tag####
  observe({
    if (is.null(input$tag5) || input$legend5 =="#FFFFFF") {return()}
    cloud<-as.data.frame(server$cloud)
    leafletProxy("map") %>% clearGroup("tag5")
    selected <- unlist(strsplit(unlist(cloud[which(cloud$label==input$tag5),2]),"\n"))
    mask<- unlist(collection[,1]) %in% selected
    saveRDS(input$legend5,"legend5")
    sample <- server$collection[mask,,drop=F]
    spots <- sample[sample[,2,drop=F]=="point",,drop=F]
    if (length(spots) != 0) {
      spots <- matrix(unlist(apply(spots,1,spots.data)), ncol=3, byrow=T)
      leafletProxy("map") %>% addCircleMarkers(lng=as.numeric(spots[,1]), lat = as.numeric(spots[,2]), layerId = spots[,3], group = "tag5",
                                               stroke = TRUE, color = input$legend5, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$legend5,
                                               fillOpacity = 0.2)
    }
    lines <- sample[sample[,2,drop=F]=="line",,drop=F]
    if (length(lines) != 0) {
      leafletProxy("map") %>% addPolylines(data=SpatialLines(matrix(lines,ncol=5)[,3]), group="tag5",
                                           stroke = TRUE, color = input$legend5, weight = 5, opacity = 0.5)
    }
    polys <- sample[sample[,2,drop=F]=="polygon",,drop=F]
    if (length(polys) != 0) {
      leafletProxy("map") %>% addPolygons(data=SpatialPolygons(matrix(polys,ncol=5)[,3]),group="tag5",
                                          stroke = TRUE, color = input$legend5, weight = 5, opacity = 0.5, fill = TRUE, fillColor = input$legend5,
                                          fillOpacity = 0.2)
    }
  })
  
  
  observe({
    if (is.null(input$tag1)) {
      updateAceEditor(session,"content1",value = 1)
    } else {
      cloud<-as.data.frame(server$cloud)
      leafletProxy("map") %>% clearGroup("tag1")
      selected <- unlist(strsplit(unlist(cloud[which(cloud$label==input$tag1),3]),"\n"))
      mask<- unlist(server$corpus[,1]) %in% selected
      sample <- server$corpus[mask,,drop=F]
      updateAceEditor(session,"content1",value = paste(sample[,2],collapse="\n\n**********\n\n"))
    }
  })
  observe({
    if (is.null(input$tag2)) {
      updateAceEditor(session,"content2",value = 1)
    } else {
      cloud<-as.data.frame(server$cloud)
      selected <- unlist(strsplit(unlist(cloud[which(cloud$label==input$tag2),3]),"\n"))
      mask<- unlist(server$corpus[,1]) %in% selected
      sample <- server$corpus[mask,,drop=F]
      updateAceEditor(session,"content2",value = paste(sample[,2],collapse="\n\n**********\n\n"))
    }
  })
  observe({
    if (is.null(input$tag3)) {
      updateAceEditor(session,"content3",value = 1)
    } else {
      cloud<-as.data.frame(server$cloud)
      selected <- unlist(strsplit(unlist(cloud[which(cloud$label==input$tag3),3]),"\n"))
      mask<- unlist(server$corpus[,1]) %in% selected
      sample <- server$corpus[mask,,drop=F]
      updateAceEditor(session,"content3",value = paste(sample[,2],collapse="\n\n**********\n\n"))
    }
  })
  observe({
    if (is.null(input$tag4)) {
      updateAceEditor(session,"content4",value = 1)
    } else {
      cloud<-as.data.frame(server$cloud)
      selected <- unlist(strsplit(unlist(cloud[which(cloud$label==input$tag4),3]),"\n"))
      mask<- unlist(server$corpus[,1]) %in% selected
      sample <- server$corpus[mask,,drop=F]
      updateAceEditor(session,"content4",value = paste(sample[,2],collapse="\n\n**********\n\n"))
    }
  })
  
  observe({
    if (is.null(input$tag5)) {
      updateAceEditor(session,"content5",value = 1)
    } else {
      cloud<-as.data.frame(server$cloud)
      selected <- unlist(strsplit(unlist(cloud[which(cloud$label==input$tag5),3]),"\n"))
      mask<- unlist(server$corpus[,1]) %in% selected
      sample <- server$corpus[mask,,drop=F]
      updateAceEditor(session,"content5",value = paste(sample[,2],collapse="\n\n**********\n\n"))
    }
  })
  
})


