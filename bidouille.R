library(leaflet)
library(htmltools)
library(htmlwidgets)

# This tells htmlwidgets about our plugin name, version, and
# where to find the script. (There's also a stylesheet argument
# if the plugin comes with CSS files.)
arrowPlugin <- htmlDependency("leaflet-arrows", "1",
                             src = c(href = "https://github.com/makinacorpus/Leaflet.LineExtremities/blob/gh-pages/"),
                             script = "leaflet.lineextremities.js"
)

# A function that takes a plugin htmlDependency object and adds
# it to the map. This ensures that however or whenever the map
# gets rendered, the plugin will be loaded into the browser.
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}


leaflet()  %>% 
       addTiles(urlTemplate="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                               attribution = NULL,
                               layerId = NULL,
                               options = tileOptions()) %>% 
       setView(-122.23, 37.75, zoom = 7) %>% registerPlugin(arrowPlugin) %>%
  # Add your custom JS logic here. The `this` keyword
  # refers to the Leaflet (JS) map object.
  onRender("function(el, x) {var arrowOptions = {
    var pointA = new L.LatLng(-122.23, 37.75);
    var pointB = new L.LatLng(-123.23, 38.75);
    var pointList = [pointA, pointB];
var layer = new L.polyLine(pointList, {
    color: 'red',
    weight: 3,
    opacity: 0.5,
    smoothFactor: 1
});
;
layer.addTo(this);
    layer.showExtremities('dotM');
                              }")



# Create original data
ABData <- data.frame(a = c("London, UK", "Oxford, UK", "Berlin, DE",
                           "Hamburg, DE", "Oslo, NO"),
                     b = c(8, 0.1, 3, 2, 1))

# Create replacements data frame
Replaces <- data.frame(from = c("UK", "DE"), to = c("England", "Germany"))

# Replace patterns and return full data frame
ABNewDF <- FindReplace(data = ABData, Var = "a", replaceData = Replaces,
                       from = "from", to = "to", exact = FALSE)

# Replace patterns and return the Var as a vector
ABNewVector <- FindReplace(data = ABData, Var = "a", replaceData = Replaces,
                           from = "from", to = "to", vector = TRUE)

x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 2, mean, trim = .2)
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

stopifnot( apply(x, 2, is.vector))


Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)))

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3), "s3")
Srs4 = Polygons(list(Sr4), "s4")

l1 = cbind(c(1,2,3),c(3,2,2))
rownames(l1) = letters[1:3]
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
rownames(l1a) = letters[1:3]
l2 = cbind(c(1,2,3),c(1,1.5,1))
rownames(l2) = letters[1:3]
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")

slots <- matrix(list(), ncol = 3, nrow = 0)

slots<-rbind(slots,c(name="a",type="p",entity=Srs1))
slots<-rbind(slots,c(name="b",type="l",entity=S1))
slots<-rbind(slots,c(name="c",type="p",entity=Srs2))
slots<-rbind(slots,c(name="d",type="p",entity=Srs3))
slots<-rbind(slots,c(name="e",type="l",entity=S2))
slots<-rbind(slots,c(name="f",type="p",entity=Srs4))


x = c(1,2,3)
v = c(2,4,5)
y = c(3,2,5)
w = c(3,1,4)

S1 <- SpatialPoints(cbind(x,y))
S2 <- SpatialPoints(list(w,v))
S <- SpatialPoints(data.frame(x,y))
S
plot(S)

observeEvent(input$collection_rows_selected, {
  selected <- collection[input$elements_rows_selected,]
  spots <- selected[selected[,2]=="1",]
  saveRDS(spots,"spots")
  lines <- selected[selected[,2]=="2",]
  saveRDS(lines,"lines")
  polys <- selected[selected[,2]=="3",]
  saveRDS(spots,"spots")}
)

  pickedPoints <- picked[picked$type=="point","layerId"]
  saveRDS(pickedPoints,"pickedPoints.Rds")
  pointsData <- ldply(server$points, data.frame)
  saveRDS(pointsData,"pointsData.Rds")
  pickedPointsData <- pointsData[pointsData$layerId %in% pickedPoints,]
  saveRDS(pickedPointsData,"pickedPointsData.Rds")
  
  pickedLines <- picked[picked$type=="line","layerId"]
  saveRDS(pickedLines,"pickedLines.Rds")
  linesData <- ldply(server$lines, data.frame)
  saveRDS(linesData,"linesData.Rds")
  pickedLinesData <- linesData[linesData$layerId %in% pickedLines,]
  saveRDS(pickedLinesData,"pickedLinesData.Rds")
  
  pickedPolygons <- picked[picked$type=="polygon","layerId"]
  saveRDS(pickedPolygons,"pickedPolygons.Rds")
  polygonsData <- ldply(server$polygons, data.frame)
  saveRDS(polygonsData,"polygonsData.Rds")
  pickedPolygonsData <- polygonsData[polygonsData$layerId %in% pickedPolygons,]
  saveRDS(pickedPolygonsData,"pickedPolygonsData.Rds")
  
  leafletProxy("map") %>% clearMarkers() %>% clearShapes()
  if (nrow(pickedPointsData)==0) {
    return
  } else {
    leafletProxy("map") %>% addCircleMarkers(lng = pickedPointsData$lng,
                                             lat = pickedPointsData$lat,
                                             layerId = pickedPointsData$layerId,
                                             stroke = input$stroke, 
                                             color = input$strokeColor, 
                                             weight = input$strokeWeight, 
                                             opacity = input$strokeOpacity, 
                                             fill = input$fill, 
                                             fillColor = input$fillColor, 
                                             fillOpacity = input$fillOpacity) 
  }
  
  
  if (nrow(pickedLinesData)==0) {
    return
  } else {
    leafletProxy("map") %>% addPolylines(lng=pickedLinesData$lng,
                                         lat=pickedLinesData$lat,
                                         layerId=unique(na.omit(pickedLinesData$layerId)),
                                         stroke = input$stroke, 
                                         color = input$strokeColor, 
                                         weight = input$strokeWeight, 
                                         opacity = input$strokeOpacity, 
                                         fill = input$fill, 
                                         fillColor = input$fillColor, 
                                         fillOpacity = input$fillOpacity)
  }
  if (nrow(pickedPolygonsData)==0) {
    return
  } else {
    leafletProxy("map") %>% addPolygons(lng=pickedPolygonsData$lng,
                                        lat=pickedPolygonsData$lat,
                                        layerId=unique(na.omit(pickedPolygonsData$layerId)),
                                        stroke = input$stroke, 
                                        color = input$strokeColor, 
                                        weight = input$strokeWeight, 
                                        opacity = input$strokeOpacity, 
                                        fill = input$fill, 
                                        fillColor = input$fillColor, 
                                        fillOpacity = input$fillOpacity)
  }
})
####Select marker####
observeEvent(input$map_marker_click, {
  if (input$elementType != 0) {
    return()
  } else {
    event<-input$map_marker_click
    client$selected<-data.frame(lng=as.double(event$lng),
                                lat=as.double(event$lat),
                                layerId=event$id,
                                pointId=event$id,
                                # user=as.character(input$elementName),
                                stringsAsFactors = FALSE)
    items<-ldply(server$elements, data.frame)
    info<-items[event$id,5]
    
    leafletProxy("map") %>% removeShape("selected") %>%
      addCircleMarkers(lng=as.double(event$lng),
                       lat=as.double(event$lat),
                       radius=10,
                       layerId = "selected",
                       stroke = TRUE,
                       color = "red",
                       weight = 5,
                       opacity = 0.5,
                       fill = TRUE,
                       fillColor = "red",
                       fillOpacity = 0.2) %>%
      addPopups(event$lng, event$lat, popup=info)
  }
})
####Select marker####

####Select shape######
observeEvent(input$map_shape_click, {
  if (input$elementType != 0) {
    return()
  } else {
    event<-input$map_shape_click
    items<-ldply(server$elements, data.frame)
    info<-items[event$id,5]
    polygons <- ldply(server$polygons, data.frame)
    lines <- ldply(server$lines, data.frame)
    shapes<-rbind(polygons,lines)
    shapes<-shapes[shapes$layerId==event$id,]
    if (input$elementName==1) {
      server$beacon1<-shapes
    } else {
      server$beacon2<-shapes
    }
    if (length(shapes$layerId==2)) {
      leafletProxy("map") %>% removeMarker("selected")  %>%
        addPolylines(lng=shapes$lng,
                     lat=shapes$lat,
                     layerId="selected",
                     stroke = TRUE,
                     color = "red",
                     weight = 5,
                     opacity = 0.5,
                     fill = TRUE,
                     fillColor = "red",
                     fillOpacity = 0.2) %>%
        addPopups(event$lng, event$lat, popup=info)
      
      
    } else {
      leafletProxy("map") %>% removeMarker("selected") %>%
        addPolygons(lng=shapes$lng,
                    lat=shapes$lat,
                    layerId="selected",
                    stroke = TRUE,
                    color = "red",
                    weight = 5,
                    opacity = 0.5,
                    fill = TRUE,
                    fillColor = "red",
                    fillOpacity = 0.2) %>%
        addPopups(event$lng, event$lat, popup=info)
      
    }
    client$selected<-data.frame(lng=as.double(event$lng),
                                lat=as.double(event$lat),
                                layerId=event$id,
                                pointId=event$id,
                                # user=as.character(input$elementName),
                                stringsAsFactors = FALSE)
  }
})
####Select shape#####

####Hide/Show####

observeEvent(input$hideMarkers, {
  points <- ldply(server$points, data.frame)
  leafletProxy("map") %>% removeMarker(points[,3])
})

observeEvent(input$showMarkers, {
  points <- ldply(server$points, data.frame)
  leafletProxy("map") %>% addCircleMarkers(lng=points[,1],
                                           lat=points[,2],
                                           radius=10,
                                           layerId = points[,3],
                                           stroke = input$stroke, 
                                           color = input$strokeColor, 
                                           weight = input$strokeWeight, 
                                           opacity = input$strokeOpacity, 
                                           fill = input$fill, 
                                           fillColor = input$fillColor, 
                                           fillOpacity = input$fillOpacity)
})

observeEvent(input$hidePolygons, {
  polygons <- ldply(server$polygons, data.frame)
  saveRDS(polygons,"polygons.Rds")
  leafletProxy("map") %>% removeShape(layerId=unique(
    na.omit(polygons$layerId)))
})

observeEvent(input$showPolygons, {
  polygons <- ldply(server$polygons, data.frame)
  leafletProxy("map") %>% addPolygons(lng=polygons$lng,
                                      lat=polygons$lat,
                                      layerId=unique(
                                        na.omit(polygons$layerId)),
                                      stroke = input$stroke, 
                                      color = input$strokeColor, 
                                      weight = input$strokeWeight, 
                                      opacity = input$strokeOpacity, 
                                      fill = input$fill, 
                                      fillColor = input$fillColor, 
                                      fillOpacity = input$fillOpacity)
})

######Hide/Show######


x = c(1)
y = c(3)
S <- SpatialPoints(cbind(x,y))


# observeEvent(client$buffer,   { # if no marker is clicked a new one is created
#   leafletProxy("map") %>% removeShape(layerId= "proto")
#   prototype <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(rbind(client$buffer[,1:2],client$buffer[1,1:2])))), "proto")), 1:1)
#   if ( head(client$buffer$layerId,1) == tail(client$buffer$layerId,1) && length(client$buffer$layerId) > 1 ) {
#     leafletProxy("map") %>% addPolygons(data=prototype,layerId=as.character(length(server$elements)+1), color = "green")
#     server$elements[[length(server$elements)+1]] <- prototype
#     client$buffer<-NULL
#   } else {
#     leafletProxy("map") %>% addPolygons(data=prototype,layerId="proto", color = "blue")
#     leafletProxy("map") %>% addCircleMarkers(lng = client$buffer$lng, lat = client$buffer$lat, layerId = client$buffer$layerId, color = "grey")    # server$elements[[event$Id]] <- event
#   }
#     })
# 
# observeEvent(client$buffer,   { # if no marker is clicked a new one is created
#   leafletProxy("map") %>% removeShape(layerId= "proto")
#     if ( head(client$buffer$layerId,1) == tail(client$buffer$layerId,1) && length(client$buffer$layerId) > 1 ) {
#     prototype <-SpatialLines(list(Lines(list(Line(cbind(as.vector(client$buffer[-nrow(client$buffer),1]),as.vector(client$buffer[-nrow(client$buffer),2])))), "proto")))
#     leafletProxy("map") %>% addPolylines(data=prototype,layerId=as.character(length(server$elements)+1), color = "green")
#     server$elements[[length(server$elements)+1]] <- prototype
#     client$buffer<-NULL
#   } else {
#     prototype <-SpatialLines(list(Lines(list(Line(cbind(as.vector(client$buffer[,1]),as.vector(client$buffer[,2])))), "proto")))
#     leafletProxy("map") %>% addPolylines(data=prototype,layerId="proto", color = "blue")
#     leafletProxy("map") %>% addCircleMarkers(lng = client$buffer$lng, lat = client$buffer$lat, layerId = client$buffer$layerId, color = "grey")    # server$elements[[event$Id]] <- event
#   }
# })
# 
# observeEvent(client$buffer,   { # if no marker is clicked a new one is created
#   leafletProxy("map") %>% removeMarker("proto")
#   if (tail(client$buffer$layerId,1)=="proto" && length(client$buffer$layerId) > 1 ) {
#     prototype <-SpatialPoints(cbind(as.vector(client$buffer[2,1]),as.vector(client$buffer[2,2])))
#     leafletProxy("map") %>% addCircleMarkers(data=prototype,layerId=as.character(length(server$elements)), color = "green")
#     server$elements[[length(server$elements)]] <- prototype
#     client$buffer<-NULL
#   } else {
#     client$buffer<-tail(client$buffer,1)
#     prototype <-SpatialPoints(cbind(as.vector(client$buffer[1,1]),as.vector(client$buffer[1,2])))
#     leafletProxy("map") %>% addCircleMarkers(data=prototype,layerId="proto", color = "grey")
#   }
# })


#Multipoints

cl1 = cbind(rnorm(1, 10), rnorm(1, 10))
cl2 = cbind(rnorm(5, 10), rnorm(5, 0))
cl3 = cbind(rnorm(7, 0), rnorm(7, 10))
mp = SpatialMultiPoints(list(cl1))
mpx = rbind(mp, mp) # rbind method
plot(mp, col = 2, cex = 1, pch = 1:3)
mp
mp[1:2]
print(mp, asWKT=TRUE, digits=3)
mpdf = SpatialMultiPointsDataFrame(list(cl1, cl2, cl3), data.frame(a = 1:3))
mpdf
mpdfx = rbind(mpdf, mpdf) # rbind method
plot(mpdf, col = mpdf$a, cex = 1:3)
as(mpdf, "data.frame")
mpdf[1:2,]

#Lines
l1 = cbind(c(1,2,3),c(3,2,2))
rownames(l1) = letters[1:3]
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
rownames(l1a) = letters[1:3]
l2 = cbind(c(1,2,3),c(1,1.5,1))
rownames(l2) = letters[1:3]
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
Sl = SpatialLines(list(S1,S2))
summary(Sl)
plot(Sl, col = c("red", "blue"))


SpatialLines(list(Lines(list(Line(l1)),"a")))
SpatialLines(LinesList, proj4string = CRS(as.character(NA)))
SpatialLinesDataFrame(sl, data, match.ID = TRUE)
as.SpatialLines.SLDF(SLDF)
getSpatialLinesMidPoints(SL)
LineLength(cc, longlat = FALSE, sum = TRUE)
LinesLength(Ls, longlat = FALSE)
SpatialLinesLengths(SL, longlat)

#Polygon
Sr1 = Polygon(cbind(c(2,1,2),c(2,3,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
plot(SpP, col = 1:3, pbg="white")
grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as(grd, "SpatialPolygons")
plot(polys)
text(coordinates(polys), labels=row.names(polys))



# leafletProxy("map") %>% addCircleMarkers(lng = as.double(input$map_marker_click$lng),lat = as.double(input$map_marker_click$lat),layerId = as.character(input$map_marker_$nonce), color = "grey")

# observeEvent(input$map_click, { # if no marker is clicked a new one is created
#   if (input$elementType != 3) {
#     return()      
#   } else { 
#     if (is.null(client$buffer)) { # we check if a polygon is already underway
#       ## if not, store its id in client$current and save spot in server$elements
#       client$current <- as.character(as.integer(length(server$elements)+1))
#       server$elements[[client$current]] <- client$current
#     } # if yes we'll add the new points to the polygon's definition
#     event <- input$map_click # we store the event's info
#     id <- as.character(as.integer(length(server$elements))+1) # new marker's id
#     saveRDS(client$current,"current.Rds")
#     client$selected <- data.frame(lng = event$lng, # the new marker's data
#                                   lat = event$lat,
#                                   layerId = id,
#                                   pointId = id,
#                                   # user = as.character(input$elementName),
#                                   stringsAsFactors = FALSE)
#     client$buffer <- rbind(client$buffer,client$selected) # added to poly data
#     ## and stored in server$points, where we will later retrieve its data
#     server$points[[length(server$points)+1]] <- client$selected
#     ## and we store its ref in server$elements
#     server$elements[[id]] <- data.frame(layerId = as.character(id),
#                                         type = "point",
#                                         element=input$elementName,
#                                         popup=as.character(tagList(
#                                           tags$strong("Name:", as.character(input$elementName)),
#                                           tags$br(),
#                                           tags$p("Tags:", as.character(input$elementTags))
#                                           # ,
#                                           # tags$br(),
#                                           # tags$p("Description:", as.character(input$elementDescription)), 
#                                           # tags$br(),
#                                           # tags$em("Author:", input$user)
#                                         )))
#     ## we display this new marker as well as a temporary polygon
#     leafletProxy("map") %>% addCircleMarkers(lng = as.double(event$lng),
#                                              lat = as.double(event$lat),
#                                              layerId = id,
#                                              color = "grey")
#     leafletProxy("map") %>% addPolygons(lng=client$buffer$lng,
#                                         lat=client$buffer$lat,
#                                         layerId="buffer",
#                                         color = "grey")
#   }
# })
# 
# observeEvent(input$map_shape_click, { # if no marker is clicked a new one is created
#   if (input$elementType != 3) {
#     return()      
#   } else { 
#     if (is.null(client$buffer)) { # we check if a polygon is already underway
#       ## if not, store its id in client$current and save spot in server$elements
#       client$current <- as.character(as.integer(length(server$elements)+1))
#       server$elements[[client$current]] <- client$current
#     } # if yes we'll add the new points to the polygon's definition
#     event <- input$map_shape_click # we store the event's info
#     id <- as.character(as.integer(length(server$elements))+1) # new marker's id
#     
#     saveRDS(client$current,"current.Rds")
#     client$selected <- data.frame(lng = event$lng, # the new marker's data
#                                   lat = event$lat,
#                                   layerId = id,
#                                   pointId = id,
#                                   # user = as.character(input$elementName),
#                                   stringsAsFactors = FALSE)
#     client$buffer <- rbind(client$buffer,client$selected) # added to poly data
#     ## and stored in server$points, where we will later retrieve its data
#     server$points[[length(server$points)+1]] <- client$selected
#     ## and we store its ref in server$elements
#     server$elements[[id]] <- data.frame(layerId = as.character(id),
#                                         type = "point",
#                                         element=input$elementName,
#                                         popup=as.character(tagList(
#                                           tags$strong("Name:", as.character(input$elementName)),
#                                           tags$br(),
#                                           tags$p("Tags:", as.character(input$elementTags))
#                                           # ,
#                                           # tags$br(),
#                                           # tags$p("Description:", as.character(input$elementDescription)), 
#                                           # tags$br(),
#                                           # tags$em("Author:", input$user)
#                                         )))
#     ## we display this new marker as well as a temporary polygon
#     leafletProxy("map") %>% addCircleMarkers(lng = as.double(event$lng),
#                                              lat = as.double(event$lat),
#                                              layerId = id,
#                                              color = "grey")
#     leafletProxy("map") %>% addPolygons(lng=client$buffer$lng,
#                                         lat=client$buffer$lat,
#                                         layerId="buffer",
#                                         color = "grey")
#   }
# })
# 
# observeEvent(input$map_marker_click, { # if a marker is clicked add it to poly
#   if (input$elementType != 3) {
#     return()
#   } else {
#     if (is.null(client$buffer)) { # we check if a new poly is already underway
#       ## if not, store its id in client$current and save spot in server$elements
#       client$current <- as.character(as.integer(length(server$elements)+1))
#       server$elements[[client$current]] <- as.character(client$current)
#     } # if yes we'll add the new points to the polygon's definition
#   }
#   event <- input$map_marker_click # we store the event's info
#   client$selected <- data.frame(lng = event$lng, # the marker's data
#                                 lat = event$lat,
#                                 layerId = event$id,
#                                 pointId = event$id,
#                                 # user = as.character(input$elementName),
#                                 stringsAsFactors = FALSE)
#   client$buffer <- rbind(client$buffer,client$selected) # added to poly's data
#   ## we update the temporary polygon's display
#   leafletProxy("map") %>% addPolygons(lng = client$buffer$lng,
#                                       lat = client$buffer$lat,
#                                       layerId = "buffer",
#                                       stroke = TRUE, 
#                                       color = "grey")
#   ## we need to 'close' the shape if its first node is clicked
#   if (nrow(client$buffer) > 1 && 
#       ## provided there are more than one node
#       head(client$buffer$pointId,1) == tail(client$buffer$pointId,1)) {
#     ## if so we remove the temporary polygon from the map
#     leafletProxy("map") %>% removeShape(layerId = "buffer")
#     ## we update the buffer/poly's data with the reserved id a
#     client$buffer <- rbind(client$buffer,NA) # add a row of NAs as a separator
#     ## we store this in server$polgons where we will later retrieve its data
#     client$buffer$layerId <- client$current  
#     
#     server$polygons[[length(server$polygons)+1]] <- client$buffer
#     ## and we add these data ref to our items list
#     server$elements[[client$current]] <- data.frame(layerId = client$current,
#                                                     type = "polygon",
#                                                     element=input$elementName,
#                                                     popup=as.character(tagList(
#                                                       tags$strong("Name:", as.character(input$elementName)),
#                                                       tags$br(),
#                                                       tags$p("Tags:", as.character(input$elementTags))
#                                                     )))
#     ## we display our finished polygon on the map                                                 
#     leafletProxy("map") %>% addPolygons(lng = client$buffer$lng,
#                                         lat = client$buffer$lat,
#                                         layerId = unique(
#                                           na.omit(client$buffer$layerId)))
#     ## we clear client$buffer & client$current so we can start new shapes 
#     client$buffer<-NULL
#     client$current<-NULL
#     saveRDS(server$points,"points.Rds")
#     saveRDS(server$lines,"lines.Rds")
#     saveRDS(server$polygons,"polygons.Rds")
#     saveRDS(server$elements,"elements.Rds")
#   } else {
#     return()
#   }
# })
####Add polygon#####

x<-20
if (( x%%2 == 0 && x<20)) {
a<-"ok"
a
  } else {
a<-"non"
a
  }
