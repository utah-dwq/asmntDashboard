### Utah Water Quality Dashboard
### Version 1

library(wqTools)
library(magrittr)
require(leaflet)
require(leaflet.extras)
require(RColorBrewer)
require(sf)
require(plyr)
require(DT)


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
levels(compiled.sites$AU_DWQCat)=append(levels(compiled.sites$AU_DWQCat),"New Impairment")
compiled.sites$AU_DWQCat[compiled.sites$new_au_imp=="YES"]="New Impairment"

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
narrow.au = unique(compiled.sites[,c("ASSESS_ID","AU_DWQCat","new_au_imp","Reviewer")])
au_poly <- merge(wqTools::au_poly, narrow.au, by="ASSESS_ID")
au_poly$au_colors = ifelse(au_poly$new_au_imp=="YES", "New Impairment", as.character(au_poly$AU_DWQCat))
au_poly$au_colors[au_poly$au_colors=="Further Investigations Needed: Further Investigations Needed"] = "More Investigation Needed"
au_poly$au_colors[au_poly$au_colors=="Insufficient Data, No Exceedances: Not Assessed"] = "Insufficient Data, No Exceedances"
au_poly$au_colors <- factor(au_poly$au_colors, levels=c(
    "New Impairment",
    "Not Supporting",
    "Insufficient Data, Exceedances",
    "More Investigation Needed",
    "Insufficient Data, No Exceedances",
    "No Evidence of Impairment",
    "Supporting"))

# Prep site data for plotting
site_coords=unique(compiled.sites[,c("ASSESS_ID","MonitoringLocationIdentifier","MonitoringLocationName","MonitoringLocationTypeName","LatitudeMeasure","LongitudeMeasure","site_colors","MLID_DWQCat","IR_MLID")])
# Renaming not necessary because you're adding your own sites manually later - we can give the popups whatever names we want.
#names(site_coords)[names(site_coords)=="MonitoringLocationIdentifier"]="locationID"
#names(site_coords)[names(site_coords)=="MonitoringLocationName"]="locationName"
#names(site_coords)[names(site_coords)=="MonitoringLocationTypeName"]="locationType"
site_coords=sf::st_as_sf(site_coords, coords=c("LongitudeMeasure","LatitudeMeasure"), crs=4326, remove=F)

# Create reviewer list (JV Note - I assigned these in the .csv which I think is how this will play out in the future.)
reviewer_list=append(as.character(unique(au_poly$Reviewer)),c("All",""))

# Original data to be in data tables
au_data_table <- unique(compiled.dat[compiled.dat$MonitoringLocationName%in%compiled.sites$MonitoringLocationName,c("IR_Year","WMU","ASSESS_ID","AUID_Descr","AUID_Loc","AU_USES",
                                                                                                                  "AU_EPACat","AU_DWQCat","Reviewer")])
au_data_table$ASSESS_ID=as.character(au_data_table$ASSESS_ID)

rem_cols <- names(au_data_table)[!names(au_data_table)%in%"ASSESS_ID"]  
site_data_table <- compiled.dat[compiled.dat$MonitoringLocationName%in%compiled.sites$MonitoringLocationName,!names(compiled.dat)%in%rem_cols]
site_data_table$ASSESS_ID=as.character(site_data_table$ASSESS_ID)

##### Need to cast parameter names & uses - these have been removed for now (I think this was killing app table speed).
site_data_table=unique(site_data_table[,c("MLID","MonitoringLocationName","ASSESS_ID","MLID_DWQCat")])

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
		column(2, selectInput("whodunit", "Select reviewer", choices = reviewer_list, selected = "")),
		column(2, fileInput("import_save", "Import saved reviews", accept=".csv")),
		column(2, style = "margin-top: 25px;", downloadButton('export_reviews', "Export reviews", tags$style(type='text/css', "button#export_reviews { margin-bottom: 9px; }")))
	),
	fluidRow(
		column(6,
			tabsetPanel(id="ui_tab",
				tabPanel("Assessment Units",
					tags$br(),
					actionButton("au_comment","Make AU Comment",style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'),
					actionButton("au_table_filter_clear","Clear table filters",style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'),
					div(DT::DTOutput("AU_data"), style = "font-size:70%")
				),
				tabPanel("Sites",
					tags$br(),
					actionButton("site_comment","Make Site Comment",style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'),
					actionButton("site_table_filter_clear","Clear table filters",style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'),
					div(DT::DTOutput("Site_data"), style = "font-size:70%")
				)
			)
		),
		column(6,
				fluidRow(h4("Individual sites will appear after zooming in to an AU. Click on AU's or sites to populate the AU and site datatable tabs to the left."),
				          #fluidRow(column(6,checkboxInput("zoom_au", label = "Enable data table zoom control"))
				          fluidRow(column(6,actionButton("reset_zoom", label = "Reset map zoom",style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%'))
				                   ),      
				          fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("aumap", height="600px"),size=2, color="#0080b7")))
		)
	)
)

##################### SERVER ######################

server <- function(input, output, session){

#  # Loading modal to keep user out of trouble while app loads & draws...
#  showModal(modalDialog(title="LOADING - PLEASE WAIT...",size="l",footer=NULL))
#  
#	# Remove modal when app is ready
#	observe({
#		req(reactive_objects$map_done, reactive_objects$site_data_table,reactive_objects$au_data_table)
#		removeModal()
#	})

  # Empty reactive values object
  reactive_objects=reactiveValues()
 
  # Select sites and polygons based on reviewer selected to populate tables & map
    observe({
      req(input$whodunit)
	  reviewer <- input$whodunit
      if(reviewer=="All"){
        reactive_objects$au_poly = au_poly
		reactive_objects$au_data_table=au_data_table
        reactive_objects$site_coords = site_coords
		reactive_objects$site_data_table=site_data_table
      }else{
		if(reviewer!=""){
          reactive_objects$au_poly = au_poly[au_poly$Reviewer==reviewer,]
		  reactive_objects$au_data_table=au_data_table[au_data_table$Reviewer==reviewer,]
          reactive_objects$site_coords = site_coords[site_coords$ASSESS_ID %in% reactive_objects$au_poly$ASSESS_ID,]
		  reactive_objects$site_data_table=site_data_table[site_data_table$ASSESS_ID %in% reactive_objects$au_poly$ASSESS_ID,]
		}
	  }
	 })
	 
  
  # Select map colors
  pal <- RColorBrewer::brewer.pal(length(unique(au_poly$au_colors)),"Spectral")
  pal1 = leaflet::colorFactor(pal, domain=au_poly$au_colors)
  	
	# AU data table output
	output$AU_data <- DT::renderDT({
		req(reactive_objects$au_data_table)
			DT::datatable(
				reactive_objects$au_data_table, selection='single', rownames=FALSE, filter="top",
					options = list(scrollY = '600px', paging = FALSE, scrollX=TRUE, dom="ltipr")
			)
		})
	outputOptions(output, "AU_data", suspendWhenHidden = FALSE)
	
	# Site data table output
	output$Site_data <- DT::renderDT({
		req(reactive_objects$site_data_table)
			DT::datatable(
				reactive_objects$site_data_table, selection='single', rownames=FALSE, filter="top",
					options = list(scrollY = '600px', paging = FALSE, scrollX=TRUE, dom="ltipr")
			)
	})
	outputOptions(output, "Site_data", suspendWhenHidden = FALSE)

  # Select au map set up
  aumap = leaflet::createLeafletMap(session, 'aumap')
  # map
    session$onFlushed(once = T, function() {
		output$aumap <- leaflet::renderLeaflet({
			req(reactive_objects$site_coords)
			map = wqTools::buildMap(plot_polys=FALSE, search="")
			map=addMapPane(map,"au_poly", zIndex = 425)
			map = addPolygons(map, data=reactive_objects$au_poly,group="Assessment units",smoothFactor=2,opacity=0.9, fillOpacity = 0.1, layerId=reactive_objects$au_poly$ASSESS_ID,weight=3,color=~pal1(reactive_objects$au_poly$au_colors), options = pathOptions(pane = "au_poly"),
			                popup=paste0(
			                  "AU name: ", reactive_objects$au_poly$AU_NAME,
			                  "<br> AU ID: ", reactive_objects$au_poly$ASSESS_ID,
			                  "<br> AU type: ", reactive_objects$au_poly$AU_Type,
			                  "<br> Category: ", reactive_objects$au_poly$au_colors)
			)
			map = addPolygons(map, data=bu_poly,group="Beneficial uses",smoothFactor=4,opacity=0.9, ,fillOpacity = 0.1,weight=3,color="green", options = pathOptions(pane = "underlay_polygons"),
			                popup=paste0(
			                  "Description: ", bu_poly$R317Descrp,
			                  "<br> Uses: ", bu_poly$bu_class)
			)
			map = addPolygons(map, data=ss_poly,group="Site-specific standards",smoothFactor=2,opacity=0.9, ,fillOpacity = 0.1,weight=3,color="blue", options = pathOptions(pane = "underlay_polygons"),
			                popup=paste0("SS std: ", ss_poly$SiteSpecif)
			)
			map=addMapPane(map,"site_markers", zIndex = 450)
			map = addCircleMarkers(map, data = reactive_objects$site_coords, lat=reactive_objects$site_coords$LatitudeMeasure, lng=reactive_objects$site_coords$LongitudeMeasure, layerId = reactive_objects$site_coords$IR_MLID,group="Sites",
				weight = 5, fill = TRUE, opacity=0.95, fillOpacity = 0.5, fillColor =~pal1(reactive_objects$site_coords$site_colors),radius = 12, color =~pal1(reactive_objects$site_coords$site_colors), options = pathOptions(pane = "site_markers"),
			                           popup = paste0(
			                             "MLID: ", reactive_objects$site_coords$MonitoringLocationIdentifier,
			                             "<br> ML Name: ", reactive_objects$site_coords$MonitoringLocationName,
			                             "<br> Type: ", reactive_objects$site_coords$locationType,
			                             "<br> Category: ", reactive_objects$site_coords$MLID_DWQCat))
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
			                       colors = unique(pal1(au_poly$au_colors)), 
			                       labels = unique(au_poly$au_colors))
		})
		reactive_objects$map_done=1
    })

    
	# Map polygon click to select an AU
	observe({
		reactive_objects$sel_au = input$aumap_shape_click$id
	})
	
	# Map site click
	observe({
		reactive_objects$sel_site = input$aumap_marker_click$id
	})
	
	# Table proxies
	au_table_proxy = DT::dataTableProxy('AU_data')
	site_table_proxy = DT::dataTableProxy('Site_data')
	
	# Clear table filter buttons
	observeEvent(input$au_table_filter_clear, ignoreInit=T,{
		au_table_proxy %>% DT::clearSearch()
	})
	
	observeEvent(input$site_table_filter_clear, ignoreInit=T,{
		site_table_proxy %>% DT::clearSearch()
	})
	
	##Filter tables for map selected AU
	observeEvent(reactive_objects$sel_au, ignoreInit=T, {
		au_table_proxy %>% DT::clearSearch() %>% DT::selectRows(NULL)  %>% DT::updateSearch(keywords = list(global = "", columns=c("","",paste(reactive_objects$sel_au))))
		site_table_proxy %>% DT::clearSearch() %>% DT::selectRows(NULL) %>% DT::updateSearch(keywords = list(global = "", columns=c("","",paste(reactive_objects$sel_au))))
	})
	
	
	# AU table row click
	observeEvent(input$AU_data_rows_selected, {
		reactive_objects$sel_au=reactive_objects$au_data_table[input$AU_data_rows_selected, "ASSESS_ID"]
	})

	# Map proxy
	aumap_proxy=leaflet::leafletProxy("aumap")
	
	# Zoom to AU on au table click
	observeEvent(reactive_objects$sel_au,{
		sel_poly=reactive_objects$au_poly[reactive_objects$au_poly$ASSESS_ID %in% reactive_objects$sel_au,]
		bbox=st_bbox(sel_poly)
		#print(bbox)
		aumap_proxy %>% leaflet::flyToBounds(lng1=paste(bbox[1]), lng2=paste(bbox[3]), lat1=paste(bbox[2]), lat2=paste(bbox[4]))
	})

	# Only show sites at a particular zoom
    observeEvent(input$aumap_zoom,{
      map_zoom <- input$aumap_zoom
	  if(map_zoom<=8){
        aumap_proxy %>% leaflet::hideGroup("Sites")
      }else{aumap_proxy %>% leaflet::showGroup("Sites")}
    })

	# Reset map zoom button
    observeEvent(input$reset_zoom,{
		bbox=st_bbox(site_coords)
		aumap_proxy %>% leaflet::flyToBounds(lng1=paste(bbox[1]), lng2=paste(bbox[3]), lat1=paste(bbox[2]), lat2=paste(bbox[4]))
    })
}

## run app
shinyApp(ui = ui, server = server)



	
	###Filter tables for map selected site
	#observeEvent(reactive_objects$sel_site, ignoreInit=T, {
	#	sel_au=site_data_table$ASSESS_ID[site_data_table$
	#	au_table_proxy %>% DT::clearSearch() %>% DT::updateSearch(keywords = list(global = "", columns=c("","","",paste(reactive_objects$sel_au))))
	#	site_table_proxy %>% DT::clearSearch() %>% DT::updateSearch(keywords = list(global = "", columns=c("","",paste(reactive_objects$sel_au))))
	#})
	#JV note - skipping for now due to mismatch btwn new IR_MLID & old MLID cols
	




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





      #revmap <- leafletProxy("aumap")
      #revmap <- setView(revmap, lng=-111.547,lat=39.54484,zoom=7)
      #revmap <- clearMarkers(revmap)
      #revmap <- clearShapes(revmap)
      #revmap = addPolygons(revmap, data=au_poly2,group="Assessment units",smoothFactor=2,fillOpacity = 0.4, layerId=au_poly2$ASSESS_ID,weight=3,color=~pal1(au_poly2$au_colors), options = pathOptions(pane = "au_poly"),
      #                     popup=paste0(
      #                       "AU name: ", au_poly2$AU_NAME,
      #                       "<br> AU ID: ", au_poly2$ASSESS_ID,
      #                       "<br> AU type: ", au_poly2$AU_Type,
      #                       "<br> Category: ", au_poly2$au_colors)
      #)
      #revmap = addCircleMarkers(revmap, data = site_coords1, lat=site_coords1$LatitudeMeasure, lng=site_coords1$LongitudeMeasure, layerId = site_coords1$locationName,group="Sites", weight = 2,fill = TRUE, fillColor =~pal1(site_coords1$site_colors),fillOpacity = 0.5,radius = 7, color =~pal1(site_coords1$site_colors), options = pathOptions(pane = "site_markers"),
      #                          popup = paste0(
      #                            "Location ID: ", site_coords1$locationID,
      #                            "<br> Name: ", site_coords1$locationName,
      #                            "<br> Type: ", site_coords1$locationType,
      #                            "<br> Category: ", site_coords1$MLID_DWQCat))
      #})


