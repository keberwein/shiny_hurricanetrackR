library(shiny); library(shinydashboard); library(leaflet); library(stringr); library(XML)
library(rvest); library(foreign); library(geojsonio); library(lubridate); library(htmltools)

#library(htmlwidgets); library(RColorBrewer); library(XML); library(plyr); library(htmltools);

ui <- dashboardPage(skin="red",
    dashboardHeader(title = "Hurricane TrackR"),
    dashboardSidebar(img(src="infla.jpg", height=300,width=201), width = 200, collapsed = TRUE),
    dashboardBody(
        fluidRow(
            infoBoxOutput("timebox", width = 3),
            infoBoxOutput("statusbox", width = 3),
            infoBoxOutput("windbox", width = 3),
            infoBoxOutput("gustbox", width = 3)
        ),
        
        
        leafletOutput("stormmap", height=600)
    )
)

server <- function(input, output, session) {
    
    stormname <- "Irma"
    
    # get GIS shapefile data and create Rdata bundle
    stormname <- "Irma"
    
    # get GIS shapefile data and create Rdata bundle
    getStorm <- function(stormname) {
        wd <- getwd()
        td <- tempdir()
        setwd(td)
        
        message("Getting file links")
        gis_doc <- read_xml("http://www.nhc.noaa.gov/gis-at.xml") %>% xmlParse()
        links <<- xmlToDataFrame(gis_doc, nodes=getNodeSet(gis_doc, "//item"))
        if (nrow(links) == 0) {
            message("Storm data not found")
            quit(save = "no", status = 1)
        }
        
        # get advisory shapefile links
        message("Getting NOAA GIS data")
        adv <- links[grep("Advisory [#0-9A-Z]+ Forecast \\[shp\\]", links$title),]
        adv <- adv[grep(stormname, adv$title),]
        
        # get advisory number
        advnum <<- regmatches(adv$title, regexpr('#[0-9]+[A-Z]?', adv$title))
        advnum <<- sub("#0?", "", advnum)
        message(paste("Current advisory", advnum, sep = " "))
        
        # get url for storm files
        surl <- adv$link[grep(stormname, adv$title)]
        
        temp <- tempfile(fileext = ".zip")
        download.file(surl, temp)
        unzip(temp)
        unlink(temp)
        
        d <- dir(td, "*_5day_pts.dbf$")
        l <- dir(td, "*_5day_lin.shp$")
        p <- dir(td, "*_5day_pts.shp$")
        s <- dir(td, "*_5day_pgn.shp$")
        w <- dir(td, "*_wwlin.shp$")
        
        storm <<- read.dbf(d)
        lin <<- file_to_geojson(l, method='local', output=':memory:')
        pts <<- file_to_geojson(p, method='local', output=':memory:')
        shp <<- file_to_geojson(s, method='local', output=':memory:')
        # check for watches and warning data
        if (length(w) > 0) {
            message("Watches/Warnings present")
            ww  <<- file_to_geojson(w, method='local', output=':memory:')
        }
        
        # get wind shapefile links
        wnd <- links[grep("Advisory [#0-9A-Z]+ Wind Field \\[shp\\]", links$title),]
        wurl <- wnd$link[grep(stormname, wnd$title)]
        
        # not all advisories have wind radius data
        if (length(wurl) > 0) {
            message("Getting NOAA wind data")
            
            temp <- tempfile(fileext = ".zip")
            download.file(wurl, temp)
            unzip(temp)
            unlink(temp)
            
            d <- dir(td, "*_forecastradii.dbf$")
            s <- dir(td, "*_forecastradii.shp$")
            
            wind <- read.dbf(d)
            radii <<- file_to_geojson(s, method='local', output=':memory:')
            
        } else {
            message("No link for wind radii")
        }
        
        unlink(dir(td))
        setwd(wd)
    }
    
    # Setup a progress bar for Shiny.
    withProgress(message = 'Getting Data from NAAO and Doing a Bunch of Other Stuff', value = 0, {
        
        incProgress(1/10)
        
        # Get storm scale and set colors.
        ss <-  c("Tropical Depression", "Tropical Storm-0", "Hurricane-1", "Hurricane-2", "Major Hurricane-3", "Major Hurricane-4", "Major Hurricane-5")
        pal <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "darkred", "black"))(length(ss))
        
        incProgress(4/10) # Progress bar incrament.
        
        # Run getStorm function defined above.
        getStorm(stormname)
        
        # Parse dates and times.
        date_dt <- str_sub(storm$ADVDATE, -11) %>% as.Date("%b %d %Y") %>% as.character()
        time_ast <- str_sub(storm$ADVDATE, 1, -20)
        ifelse(length(time_ast) == 8, time_ast <- paste0("0", time_ast), time_ast)
        time_ast <- str_sub(strptime(time_ast, "%I%M %p" ), -8)
        
        # Arrange and add a few new columns.
        storm <- storm %>% arrange(TAU) %>% 
            mutate(status = paste(TCDVLP, SSNUM, sep='-'),
                   color = as.character(factor(status, levels = ss, labels = pal)),
                   advisory = ymd_hms(paste0(date_dt, " ", time_ast), tz="US/Eastern"))
        
        # Output boxes for Shiny.
        output$timebox <- renderInfoBox({
            infoBox(
                "Most Recent Advisory", paste0(storm$advisory[1]), icon = icon("refresh"),
                color = "blue"
            )
        })
        
        output$statusbox <- renderInfoBox({
            infoBox(
                "Most Recent Status", storm$status[1], icon = icon("dot-circle-o"),
                color = "green"
            )
        })
        
        output$windbox <- renderInfoBox({
            infoBox(
                "Sustained Wind Speed", paste0(round(storm$MAXWIND[1]*1.15077945, 0) , " mph."), icon = icon("superpowers"),
                color = "yellow"
            )
        })
        
        output$gustbox <- renderInfoBox({
            infoBox(
                "Max Wind Gust", paste0(round(storm$GUST[1]*1.15077945, 0), " mph."), icon = icon("life-ring"),
                color = "red"
            )
        })
        
        incProgress(6/10) # Progress bar.
        
        # Elements for leaflet map.
        title <- paste("Storm", stormname, sep = " ")
        atime <- paste("Adv", storm$ADVISNUM[1], as.character(storm$advisory[1]), "EST", sep = " ")
        if (!exists("radii")) { atime = paste(atime, "(no winds)", sep = " ")}
        rtime = paste("NEXRAD", format(Sys.time(), "%r"), sep = " ")
        
        output$stormmap <- renderLeaflet({
            m <-leaflet(data=storm) %>%
                addTiles(options = tileOptions(detectRetina = TRUE)) %>%
                addWMSTiles(
                    "http://nowcoast.noaa.gov/arcgis/services/nowcoast/radar_meteo_imagery_nexrad_time/MapServer/WmsServer",
                    layers = "1",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE),
                    attribution = "Weather data: nowcoast.noaa.gov"
                ) %>%
                addGeoJSON(shp, stroke = TRUE, color = 'grey', fill = FALSE) %>%
                addGeoJSON(lin, weight = 2, fill = FALSE) %>%
                addLegend("bottomright", colors = pal, labels = ss, title = title) %>%
                addLegend("topright", colors = NULL, labels = NULL, title = atime) %>%
                addLegend("topright", colors = NULL, labels = NULL, title = rtime)
            
            # add wind radii if available in advisory
            if (exists("radii")) {
                m <- addGeoJSON(m, radii, color = 'grey', opacity = 1, stroke = TRUE, weight = 1)
            }
            
            if (exists("ww")) {
                m <- addGeoJSON(m, ww, color = 'red', fill = FALSE)
            }
            
            incProgress(8/10, detail = "Building Doom Map") # Shiny Progress bar
            
            
            m <- addCircles(m, lng = ~LON, lat = ~LAT, radius = ~MAXWIND * 250, color = ~color,
                            opacity = 1, weight = 2, fill = TRUE, fillColor = ~color,
                            popup = ~sprintf("Time: %s %s<br/>
                                             Status: <strong>%s</strong><br/>
                                             Position: %3.2f, %3.2f<br/>
                                             Wind: %s kts<br/>
                                             Gust: %s kts",
                                             htmlEscape(DATELBL), htmlEscape(TIMEZONE),
                                             htmlEscape(status),
                                             htmlEscape(LON), htmlEscape(LAT),
                                             htmlEscape(MAXWIND),
                                             htmlEscape(GUST))) %>%
                setView(-75, 21, 5)
            
        })
        
    })
}

shinyApp(ui, server)