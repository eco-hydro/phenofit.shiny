#Libraries
library(leaflet)
library(raster)
library(shinydashboard)
library(shiny)

#Input data
# download.file("https://www.dropbox.com/s/y9ekjod2pt09rvv/test.nc?dl=0", destfile="test.nc")
inputFile    = "test.nc"
inputVarName = "Depth"
lldepth <- raster(inputFile, varname=inputVarName)
lldepth[Which(lldepth<=0, cells=T)] <- NA #Set all cells <=0 to NA
ext <- extent(lldepth)
resol <- res(lldepth)
prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lldepth) <- prj

#Project for leaflet
depth <- projectRasterForLeaflet(lldepth, "bilinear")

# options = leafletOptions(crs = leafletCRS("L.CRS.EPSG4326", "EPSG:4326"))

#Prepare UI
# sbwidth=200
# sidebar <- dashboardSidebar(width=sbwidth)
sbwidth=0
body <- dashboardBody(
    #https://stackoverflow.com/questions/31278938/how-can-i-make-my-shiny-leafletoutput-have-height-100-while-inside-a-navbarpa
    box(
        div(class="outer",width = NULL, solidHeader = TRUE,
            tags$style(type = "text/css",
                paste0(".outer {position: fixed; top: 50px; left: ", sbwidth, "px; right: 0; bottom: 0px; overflow: hidden; padding: 0}")),
            leafletOutput("map", width = "100%", height = "100%")
        )
    )
)

ui = fluidPage( fluidRow( body ) )

# ui <- dashboardPage(
#     dashboardHeader(title = "A title"),
#     # sidebar,
#     body
# )

#Server instance
server <- function(input, output, session) {
    output$map <- renderLeaflet({#Set extent
        # options = options
        leaflet()  %>%
            fitBounds(ext[1], ext[3], ext[2], ext[4])
    })

    observe({#Observer to show Popups on click
        click <- input$map_click
        if (!is.null(click)) {
            showpos(x=click$lng, y=click$lat)
        }
    })

    showpos <- function(x=NULL, y=NULL) {#Show popup on clicks
        #Translate Lat-Lon to cell number using the unprojected raster
        #This is because the projected raster is not in degrees, we cannot use it!
        cell <- cellFromXY(lldepth, c(x, y))
        if (!is.na(cell)) {#If the click is inside the raster...
            xy <- xyFromCell(lldepth, cell) #Get the center of the cell
            x <- xy[1]
            y <- xy[2]
            #Get row and column, to print later
            rc <- rowColFromCell(lldepth, cell)
            #Get value of the given cell
            val = depth[cell]
            # browser()

            content <- paste0(  "X=",rc[1],
                              "; Y=",rc[2],
                              "; Lon=", round(x, 5),
                              "; Lat=", round(y, 5),
                              "; Depth=", round(val, 1), " m")
            proxy <- leafletProxy("map")
            #add Popup
            proxy %>% clearPopups() %>% addPopups(x, y, popup = content)
            #add rectangles for testing
            proxy %>% clearShapes() %>% addRectangles(x-resol[1]/2, y-resol[2]/2, x+resol[1]/2, y+resol[2]/2)
        }
    }

    #Plot the raster
    leafletProxy("map") %>%
        addRasterImage(depth, opacity=0.8, project=FALSE, group="Example", layerId="Example",
                       colors=colorNumeric(terrain.colors(10), values(depth), na.color = "black"))
}

print(shinyApp(ui, server, options = list(port = 81)))
