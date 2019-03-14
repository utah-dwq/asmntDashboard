### Utah Water Quality Dashboard
### Jake Vander Laan, Utah DWQ, jvander@utah.gov
### Version 1

library(wqTools)
library(magrittr)
require(leaflet)
require(leaflet.extras)
require(RColorBrewer)
require(sf)
require(plyr)
require(DT)
require(shinyBS)


# Testing/toy data
compiled.dat <- read.csv("compiled_assessment_file_final2016ir.csv")
compiled.dat <- compiled.dat[!compiled.dat$MLID_DWQCat=="do not report",]
master.site.file <- read.csv("wqp_master_site_file.csv")

# Make names same as master site file so can be merged.
names(compiled.dat)[names(compiled.dat)=="MLID_NAME"] <- "MonitoringLocationName"
names(compiled.dat)[names(compiled.dat)=="AUID"] <- "ASSESS_ID"
compiled.dat$ASSESS_ID = as.factor(paste(compiled.dat$ASSESS_ID, "00", sep="_"))

# Isolate accepted sites from master site file for example
compiled.dat <- merge(compiled.dat,unique(master.site.file[,c("IR_FLAG","MonitoringLocationName")]), all.x = TRUE)
compiled.dat <- compiled.dat[!is.na(compiled.dat$IR_FLAG)&compiled.dat$IR_FLAG=="ACCEPT",]

# Keep only sites that have a match within the master site file (MLID conventions totally different, so went with Monitoring Location Name)
# EH note: I think there are some mis-matched AU/site combos between the master site file and the compiled file. I got rid of those for now (you'll see the number of rows decrease after this next line). Merging on all leads to odd NA lat/long for sites (combos that don't exist in master site file).
compiled.sites <- merge(compiled.dat,master.site.file, by=c("ASSESS_ID","MonitoringLocationName"))

# Sample Not Supporting AUs for "New Impairment" demo
ns_aus <- compiled.sites[compiled.sites$AU_DWQCat=="Not Supporting"&compiled.sites$MLID_DWQCat=="Not Supporting",] # Need to also have a not supporting site to be deemed a "new" impairment
uniq_ns_aus <- unique(ns_aus$ASSESS_ID)
new_au_imp = sample(uniq_ns_aus, 10) # randomly pick 10 AU's to be "newly impaired" for demo
compiled.sites$new_au_imp = ifelse(compiled.sites$ASSESS_ID%in%new_au_imp,"YES","NO")

# Sample Not Supporting sites within "newly impaired" AU's to be "newly impaired" sites.
compiled_newimp = compiled.sites[compiled.sites$new_au_imp=="YES"&compiled.sites$MLID_DWQCat=="Not Supporting",]
samplesite <- function(x){as.character(sample(x$MonitoringLocationIdentifier,1))}
new_site_imp = ddply(compiled_newimp,c("ASSESS_ID"),.fun=samplesite)
compiled.sites$new_site_imp = ifelse(compiled.sites$MonitoringLocationIdentifier%in%new_site_imp$V1, "YES","NO")

# Change identifying info for site map labels/colors 
compiled.sites$site_colors = ifelse(compiled.sites$new_site_imp=="YES", "New Impairment", as.character(compiled.sites$MLID_DWQCat))
compiled.sites$site_colors[compiled.sites$site_colors=="Further Investigations Needed: Further Investigations Needed"] = "More Investigation Needed"
compiled.sites$site_colors[compiled.sites$site_colors=="Insufficient Data, No Exceedances: Not Assessed"] = "Insufficient Data, No Exceedances"
compiled.sites$site_colors <- factor(compiled.sites$site_colors, levels=c(
  "New Impairment",
  "Not Supporting",
  "Insufficient Data, Exceedances",
  "More Investigation Needed",
  "Insufficient Data, No Exceedances",
  "No Evidence of Impairment",
  "Supporting"))

# Get AU data for polygon drawing
narrow.au = unique(compiled.sites[,c("ASSESS_ID","AU_DWQCat","new_au_imp")])
au_poly1 <- merge(wqTools::au_poly, narrow.au, by="ASSESS_ID")
au_poly1$au_colors = ifelse(au_poly1$new_au_imp=="YES", "New Impairment", as.character(au_poly1$AU_DWQCat))
au_poly1$au_colors[au_poly1$au_colors=="Further Investigations Needed: Further Investigations Needed"] = "More Investigation Needed"
au_poly1$au_colors[au_poly1$au_colors=="Insufficient Data, No Exceedances: Not Assessed"] = "Insufficient Data, No Exceedances"
au_poly1$au_colors <- factor(au_poly1$au_colors, levels=c(
    "New Impairment",
    "Not Supporting",
    "Insufficient Data, Exceedances",
    "More Investigation Needed",
    "Insufficient Data, No Exceedances",
    "No Evidence of Impairment",
    "Supporting"))

# Prep site data for plotting
site_coords=unique(compiled.sites[,c("MonitoringLocationIdentifier","MonitoringLocationName","MonitoringLocationTypeName","LatitudeMeasure","LongitudeMeasure","site_colors","MLID_DWQCat","IR_MLID")])
names(site_coords)[names(site_coords)=="MonitoringLocationIdentifier"]="locationID"
names(site_coords)[names(site_coords)=="MonitoringLocationName"]="locationName"
names(site_coords)[names(site_coords)=="MonitoringLocationTypeName"]="locationType"
site_coords1=sf::st_as_sf(site_coords, coords=c("LongitudeMeasure","LatitudeMeasure"), crs=4326, remove=F)

# Create reviewer list
wmu_assign <- as.data.frame(unique(compiled.sites$WMU))
names(wmu_assign)[names(wmu_assign)=="unique(compiled.sites$WMU)"] <- "WMU"
wmu_assign$num <- sample(1:3,10, replace=TRUE)
wmu_assign$reviewer <- "Jake"
wmu_assign$reviewer[wmu_assign$num==2] <- "Elise"
wmu_assign$reviewer[wmu_assign$num==3] <- "Emilie"

review_assign <- merge(compiled.sites[,c("WMU","ASSESS_ID","IR_MLID")], wmu_assign)

# Original data to be in data tables
au_data_table <- unique(compiled.dat[compiled.dat$MonitoringLocationName%in%compiled.sites$MonitoringLocationName,c("IR_Year","WMU","Agency","ASSESS_ID","AUID_Descr","AUID_Loc","AU_USES",
                                                                                                                  "AU_EPACat","AU_DWQCat")])

rem_cols <- names(au_data_table)[!names(au_data_table)%in%"ASSESS_ID"]  
site_data_table <- compiled.dat[compiled.dat$MonitoringLocationName%in%compiled.sites$MonitoringLocationName,!names(compiled.dat)%in%rem_cols]

##################### UI ######################
ui <-fluidPage(

	# Header
	headerPanel(title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85)),
		windowTitle="Water Quality Dashboard"),

	# Title
	titlePanel("",
		tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"),
		tags$title("Water Quality Dashboard"))
	),

	#,

	# Input widgets
	fluidRow(
		column(6,
        fluidRow(selectInput("whodunit","Reviewer Name", choices = c("All sites", "Elise","Jake","Emilie"), selected = "All sites")),
				tabsetPanel(id="ui_tab",
				  tabPanel("Assessment Units",
				          tags$br(),
				           bsButton("au_comment","Make AU Comment",style='primary'),
				           DT::dataTableOutput("AUdata"),
				           textAreaInput("au_combox", label = "Make AU Comment Here")),
					tabPanel("Sites",
					         tags$br(),
					         bsButton("site_comment","Make Site Comment",style='primary'),
					         DT::dataTableOutput("Sitedata"),
					         textAreaInput("site_combox", label = "Make Site Comment Here"))
				)),
		column(6,
				fluidRow(h4("Individual sites will appear after zooming in to an AU. Click on AU's or sites to populate the AU and site datatable tabs to the left."),
				          fluidRow(column(6,checkboxInput("zoom_au", label = "Enable data table zoom control"))
				                   ),      
				          fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("aumap", height="600px"),size=2, color="#0080b7")))
)
	)
)

##################### SERVER ######################

server <- function(input, output, session){

  # Loading modal to keep user out of trouble while map draws...
  showModal(modalDialog(title="MAP LOADING - PLEASE WAIT...","Please wait for map to draw before proceeding.",size="l",footer=NULL))
  
  # Remove modal when app is ready
  observe({
  req(aumap)
  removeModal()
  })

	# Empty reactive values object
	reactive_objects=reactiveValues()
	
	# AU centroids
	au_centroids=suppressWarnings(sf::st_centroid(au_poly1))
	au_centroids=cbind(au_centroids,sf::st_coordinates(au_centroids))
	
	# initial datatable data - AU
	observe({
	  map_bounds <- input$aumap_bounds
	  map_bounds <- input$aumap_bounds
	  if (is.null(map_bounds)){return()}
	  #print(map_bounds)
	})

	# Select au map set up
    aumap = leaflet::createLeafletMap(session, 'aumap')
  
  # Select map colors
    pal <- RColorBrewer::brewer.pal(length(unique(au_poly1$au_colors)),"Spectral")
    pal1 = leaflet::colorFactor(pal, domain=au_poly1$au_colors)
  
  # Only show sites at a particular zoom
    observeEvent(input$aumap_zoom,{
      map_zoom <- input$aumap_zoom
      if(map_zoom<=9){
        leafletProxy("aumap") %>% leaflet::hideGroup("Sites")
      }else{leafletProxy("aumap") %>% leaflet::showGroup("Sites")}
    })

  # Original map
    session$onFlushed(once = T, function() {
		output$aumap <- leaflet::renderLeaflet({
			map = wqTools::buildMap(plot_polys=FALSE)
			map = addPolygons(map, data=au_poly1,group="Assessment units",smoothFactor=2,fillOpacity = 0.4, layerId=au_poly1$ASSESS_ID,weight=3,color=~pal1(au_poly1$au_colors), options = pathOptions(pane = "au_poly"),
			                popup=paste0(
			                  "AU name: ", au_poly1$AU_NAME,
			                  "<br> AU ID: ", au_poly1$ASSESS_ID,
			                  "<br> AU type: ", au_poly1$AU_Type,
			                  "<br> Category: ", au_poly1$au_colors)
			)
			map = addPolygons(map, data=bu_poly,group="Beneficial uses",smoothFactor=4,fillOpacity = 0.1,weight=3,color="green", options = pathOptions(pane = "underlay_polygons"),
			                popup=paste0(
			                  "Description: ", bu_poly$R317Descrp,
			                  "<br> Uses: ", bu_poly$bu_class)
			)
			map = addPolygons(map, data=ss_poly,group="Site-specific standards",smoothFactor=2,fillOpacity = 0.2,weight=3,color="blue", options = pathOptions(pane = "underlay_polygons"),
			                popup=paste0("SS std: ", ss_poly$SiteSpecif)
			)
      map=addMapPane(map,"site_markers", zIndex = 450)
			map = addCircleMarkers(map, data = site_coords, lat=site_coords$LatitudeMeasure, lng=site_coords$LongitudeMeasure, layerId = site_coords$locationName,group="Sites", weight = 2,fill = TRUE, fillColor =~pal1(site_coords$site_colors),fillOpacity = 0.5,radius = 7, color =~pal1(site_coords$site_colors), options = pathOptions(pane = "site_markers"),
			                           popup = paste0(
			                             "Location ID: ", site_coords$locationID,
			                             "<br> Name: ", site_coords$locationName,
			                             "<br> Type: ", site_coords$locationType,
			                             "<br> Category: ", site_coords$MLID_DWQCat))
			map = leaflet::addLayersControl(map,
			                              position ="topleft",
			                              baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites","Assessment units","Beneficial uses", "Site-specific standards"),
			                              options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=TRUE))
			map=showGroup(map, "Assessment units")
			#map=hideGroup(map, "Sites")
			map=hideGroup(map, "Site-specific standards")
			map=hideGroup(map, "Beneficial uses") 
			map=removeMeasure(map)
			map=leaflet::addLegend(map, position = 'topright',
			                       colors = unique(pal1(au_poly1$au_colors)), 
			                       labels = unique(au_poly1$au_colors))
		})
    })

    
    # Select sites and polygons based on reviewer selected
    observe({
      reviewer <- input$whodunit
      if(reviewer=="All sites"){
        au_poly2 = au_poly1
        site_coords1 = site_coords
      }else{
        review_dat <- review_assign[review_assign$reviewer==reviewer,]
        review_sites <- review_dat$IR_MLID
        review_aus <- unique(review_dat$ASSESS_ID)
        au_poly2 = au_poly1[au_poly1$ASSESS_ID%in%review_aus,]
        site_coords1 = site_coords[site_coords$IR_MLID%in%review_sites,]
      }
      revmap <- leafletProxy("aumap")
      revmap <- setView(revmap, lng=-111.547,lat=39.54484,zoom=7)
      revmap <- clearMarkers(revmap)
      revmap <- clearShapes(revmap)
      revmap = addPolygons(revmap, data=au_poly2,group="Assessment units",smoothFactor=2,fillOpacity = 0.4, layerId=au_poly2$ASSESS_ID,weight=3,color=~pal1(au_poly2$au_colors), options = pathOptions(pane = "au_poly"),
                           popup=paste0(
                             "AU name: ", au_poly2$AU_NAME,
                             "<br> AU ID: ", au_poly2$ASSESS_ID,
                             "<br> AU type: ", au_poly2$AU_Type,
                             "<br> Category: ", au_poly2$au_colors)
      )
      revmap = addCircleMarkers(revmap, data = site_coords1, lat=site_coords1$LatitudeMeasure, lng=site_coords1$LongitudeMeasure, layerId = site_coords1$locationName,group="Sites", weight = 2,fill = TRUE, fillColor =~pal1(site_coords1$site_colors),fillOpacity = 0.5,radius = 7, color =~pal1(site_coords1$site_colors), options = pathOptions(pane = "site_markers"),
                                popup = paste0(
                                  "Location ID: ", site_coords1$locationID,
                                  "<br> Name: ", site_coords1$locationName,
                                  "<br> Type: ", site_coords1$locationType,
                                  "<br> Category: ", site_coords1$MLID_DWQCat))
      })
    
 #### Working on data table stuff #####   
    # Bounding function
    in_bounding_box <- function(data, lat, long, bounds) {
      data %>%
        dplyr::filter(
          lat > bounds$south &
            lat < bounds$north &
            long < bounds$east & long > bounds$west
        )
    }
    
    # data_map <- reactive({
    #   if (is.null(input$aumap_bounds)) {
    #     NULL
    #   } else {
    #     bounds <- input$aumap_bounds
    #     blah <- in_bounding_box(site_coords, site_coords$LatitudeMeasure, site_coords$LatitudeMeasure, bounds)
    #      }
    # })
    
	# Map polygon click
	observe({
		au_click <- input$aumap_shape_click
		if (is.null(au_click)){return()}
		auid=au_click$id
		reactive_objects$sel_au = auid
		reactive_objects$sel_site = site_data_table$MonitoringLocationName[site_data_table$ASSESS_ID%in%auid]})
	
	# Map site click
	observe({
	  site_click <- input$aumap_marker_click
	  if (is.null(site_click)){return()}
	  siteid=site_click$id
	  reactive_objects$sel_site = siteid
	  reactive_objects$sel_au = site_data_table$ASSESS_ID[site_data_table$MonitoringLocationName==siteid][1]
	})
		
		output$AUdata <- DT::renderDataTable({
		  req(aumap)
		  if(is.null(input$aumap_shape_click)&is.null(input$aumap_marker_click)){au_dattab=au_data_table}else{
		    au_dattab <- au_data_table[au_data_table$ASSESS_ID%in%reactive_objects$sel_au,]
		  }
		  DT::datatable(
		    au_dattab,
		    rownames=FALSE,
		    style = "bootstrap",
		    class = "compact",
		    filter = 'top',
		    width = "100%",
		    options = list(
		      pageLength = 100,
		      scrollX = TRUE,
		      scrollY = 400,
		      dom = 't')
		    )
		})
		
		output$Sitedata <- DT::renderDataTable({
		  req(aumap)
		  if(is.null(input$aumap_shape_click)&is.null(input$aumap_marker_click)){site_dattab=site_data_table}else{
		    site_dattab <- site_data_table[site_data_table$MonitoringLocationName%in%reactive_objects$sel_site,]
		  }
		  DT::datatable(
		    site_dattab,
		    rownames=FALSE,
		    style = "bootstrap",
		    class = "compact",
		    filter = 'top',
		    width = "100%",
		    options = list(
		      pageLength = 100,
		      scrollX = TRUE,
		      scrollY = 400,
		      dom = 't'
		    )
		  )
		})

observeEvent(input$Sitedata_rows_selected,{
  updateButton(session, "site_comment", style='danger')
})

}


## run app
shinyApp(ui = ui, server = server)

# observeEvent(input$zoom_au,{
#     map_bounds <- input$aumap_bounds
#     if (is.null(map_bounds)){return()}

#     print(map_bounds)
# })

# sitemap <- leafletProxy("aumap")
# sitemap = setView(sitemap, lng = reactive_objects$aulng, lat = reactive_objects$aulat, zoom = 12)
# sitemap=showGroup(sitemap, "Sites")
# sitemap = addCircleMarkers(aumap, data = site_coords, lat=site_coords$LatitudeMeasure, lng=site_coords$LongitudeMeasure, group="Sites", fill = FALSE, radius = 7, color =~pal1(site_coords$site_colors), options = pathOptions(pane = "markers"),
#                            popup = paste0(
#                              "Location ID: ", site_coords$locationID,
#                              "<br> Name: ", site_coords$locationName,
#                              "<br> Type: ", site_coords$locationType,
#                              "<br> Lat: ", site_coords$LatitudeMeasure,
#                              "<br> Long: ", site_coords$LongitudeMeasure,
#                              "<br> Category: ", site_coords$MLID_DWQCat))
# sitemap = hideGroup(sitemap, "AU Info")
#print(reactive_objects$aulat)

# #Map marker click (to identify selected site)
# observe({
# 	site_click <- input$sitemap_marker_click
# 	if (is.null(site_click)){return()}
# 	siteid=site_click$id
# 	reactive_objects$sel_mlid=siteid
# 	print(site_click$id)
# })
# 

