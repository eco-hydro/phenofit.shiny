library(shiny)
library(leaflet)

latitude <- c(35.94077, 35.83770, 35.84545, 35.81584, 35.79387, 36.05600)
longitude <- c(-78.58010, -78.78084, -78.72444, -78.62568, -78.64262, -78.67600)
radius<-c(15, 12, 12, 12, 12, 15)
ids<-c("a", "b", "c", "d", "e", "f")

shinyApp(
    ui = fluidPage(
        fluidRow(
            leafletMap(
                "map", "100%", 400,
                initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                options=list(
                    center = c(37.45, -93.85),
                    zoom = 4,
                    maxBounds = list(list(17, -180), list(59, 180))))),
        fluidRow(verbatimTextOutput("Click_text"))),
    server = function(input, output, session){
        map = createLeafletMap(session, 'map')
        session$onFlushed(once=T, function(){
            map$addCircleMarker(lat = latitude,
                                lng = longitude,
                                radius = radius,
                                layerId=ids)
        })

        observe({
            click<-input$map_marker_click
            if(is.null(click))
                return()
            text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
            text2<-paste("You've selected point ", click$id)
            map$clearPopups()
            map$showPopup( click$lat, click$lng, text)
            output$Click_text<-renderText({
                text2
            })
        })

        observe({
            pos <- input$MAPID_click
            print(pos)
            browser()
        })
    }
)
