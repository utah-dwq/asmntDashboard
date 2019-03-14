#' Build a site map of WQP sites or ECHO facilities
#' 
#' Build a map of sample sites, facilities, or both. Map includes sites, beneficial use and assessment unit polygons, and satellite and topo baselayers.
#' This is designed to work with column names as extracted from WQP or ECHO via udwqTools functions readWQP() and readECHO_fac(). Map will launch in default browser (or R-Studio's browser if using R-Studio).
#' Site and assessment unit features are searchable by idetifier and name via the search button on the left side of the map.
#' The most recently turned on layer is "on top" of the map. Only features on top will show their pop-up on click.
#' @param fac Facility locations queried via readECHO_fac.
#' @param sites Site locations queried via readWQP(type="sites"). May also be a data file with WQP site information merged to it.
#' @param au_poly Optional. Polygon file to be mapped as assessment units. Useful for mapping a subset of specific assessment units. If missing, the default state wide AU polygon is used.
#' @param bu_poly Optional. Polygon file to be mapped as beneficial uses. Useful for mapping a subset of beneficial uses. If missing, the default state wide uses polygon is used.
#' @param ss_poly Optional. Polygon file to be mapped as site specific standards. Useful for mapping a subset of ss polygons. If missing, the default state wide ss polygon is used.
#' @param search Vector of objects to be made searchable. One or both of "sites" and "aus". Defaults to c("sites","aus"). Any other inputs are ignored.
#' @import leaflet
#' @importFrom leaflet.extras addSearchFeatures
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sf st_centroid
#' @importFrom sf st_coordinates
#' @examples
#' # Read sites & facility locations
#' jr_sites=readWQP(type="sites",
#' 	siteid=c("UTAHDWQ_WQX-4994100","UTAHDWQ_WQX-4994120","UTAHDWQ_WQX-4991860",
#' 			 "UTAHDWQ_WQX-4994190","UTAHDWQ_WQX-4994172","UTAHDWQ_WQX-4994090",
#' 			 "UTAHDWQ_WQX-4992890","UTAHDWQ_WQX-4992880","UTAHDWQ_WQX-4992480",
#' 			 "UTAHDWQ_WQX-4992055","UTAHDWQ_WQX-4991940","UTAHDWQ_WQX-4991880"))		 
#' jr_fac=readECHO_fac(p_pid=c("UT0024392","UT0024384","UT0025852","UT0021725"))
#' #Build some maps
#' map1=buildMap(sites=jr_sites, fac=jr_fac) #define new object for use later
#' map1 #call generated map object to launch in browser
#' buildMap(sites=jr_sites) #just sites, launch w/o generating map object in workspace
#' buildMap(fac=jr_fac) #just facilities
#' buildMap() #Build an empty map w/ just AU, BU, and SS std polys
#' #html maps can be saved via htmlwidgets package saveWidget(map1, file="your/path/map1.html")

#' @export
buildMap=function(fac, sites, au_poly, bu_poly, ss_poly, search=c("sites","aus"), plot_polys=TRUE){
	
	if(missing(au_poly)){get(data("au_poly", envir = environment()))}
	if(missing(bu_poly)){get(data("bu_poly", envir = environment()))}
	if(missing(ss_poly)){get(data("ss_poly", envir = environment()))}
	au_centroids=suppressWarnings(sf::st_centroid(au_poly))
	au_centroids=cbind(au_centroids,sf::st_coordinates(au_centroids))
	
	# AU polygon colors
	pal <- RColorBrewer::brewer.pal(length(unique(au_poly$au_colors)),"Spectral")
	pal1 = leaflet::colorFactor(pal, domain=au_poly$au_colors)
	
	
	if(missing(fac) & missing(sites)){
		#Build empty map
		
		map=leaflet::leaflet(au_poly)
			map=leaflet::addProviderTiles(map, "Esri.WorldTopoMap", group = "Topo")
			map=leaflet::addProviderTiles(map,"Esri.WorldImagery", group = "Satellite")
			
			map=addMapPane(map,"underlay_polygons", zIndex = 410)
			map=addMapPane(map,"au_poly", zIndex = 415)
			map=addMapPane(map,"markers", zIndex = 420)

			map=leaflet::addCircles(map, lat=au_centroids$Y, lng=au_centroids$X, group="au_names", label=au_centroids$AU_NAME, stroke=F, fill=F,
				popup = paste0(
					"AU ID: ", au_centroids$ASSESS_ID,
					"<br> AU Name: ", au_centroids$AU_NAME,
					"<br> AU Type: ", au_centroids$AU_Type,
					"<br> Category: ", au_centroids$AU_DWQCat,
					"<br> New Impairment: ", au_centroids$new_au_imp))
			map=leaflet::addCircles(map, lat=au_centroids$Y, lng=au_centroids$X, group="au_ids", label=au_centroids$ASSESS_ID, stroke=F, fill=F,
				popup = paste0(
					"AU name: ", au_centroids$AU_NAME,
					"<br> AU ID: ", au_centroids$ASSESS_ID,
					"<br> AU type: ", au_centroids$AU_Type,
					"<br> Category: ", au_centroids$AU_DWQCat,
					"<br> New Impairment: ", au_centroids$new_au_imp))
			
			if(plot_polys){
			  
				map=addPolygons(map, data=bu_poly,group="Beneficial uses",smoothFactor=4,fillOpacity = 0.1,weight=3,color="green", options = pathOptions(pane = "underlay_polygons"),
					popup=paste0(
						"Description: ", bu_poly$R317Descrp,
						"<br> Uses: ", bu_poly$bu_class)
					)
				
				map=addPolygons(map, data=au_poly,group="Assessment units",smoothFactor=2,fillOpacity = 0.5, layerId=au_poly$ASSESS_ID,weight=3,color=~pal1(au_poly$au_colors), options = pathOptions(pane = "au_poly"),
					popup=paste0(
						"AU name: ", au_poly$AU_NAME,
						"<br> AU ID: ", au_poly$ASSESS_ID,
						"<br> AU type: ", au_poly$AU_Type,
						"<br> Category: ", au_poly$AU_DWQCat,
						"<br> New Impairment: ", au_poly$new_au_imp)
					)
				map=addPolygons(map, data=ss_poly,group="Site-specific standards",smoothFactor=4,fillOpacity = 0.1,weight=3,color="blue", options = pathOptions(pane = "underlay_polygons"),
					popup=paste0("SS std: ", ss_poly$SiteSpecif)
					)
				map=leaflet::addLayersControl(map,
					position ="topleft",
					baseGroups = c("Topo","Satellite"),overlayGroups = c("Assessment units","Beneficial uses", "Site-specific standards"),
					options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))
				#map=hideGroup(map, "Assessment units")
				map=hideGroup(map, "Site-specific standards")
				map=hideGroup(map, "Beneficial uses")
				
				map=leaflet::addLegend(map, position = 'topright',
				                       colors = unique(pal1(au_poly$au_colors)), 
				                       labels = unique(au_poly$au_colors))

			}
			
			if("aus" %in% search){
				map=addSearchFeatures(map,
					targetGroups = c('au_ids','au_names'),
					options = leaflet.extras::searchFeaturesOptions(
					zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
					autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))
			}
			
			map=leaflet::addMeasure(map, position="topright")

	}else{
		if(!missing(sites)){
		  # site colors
		  # pals <- RColorBrewer::brewer.pal(length(unique(sites$site_colors)),"Spectral")
		  # pals1 = leaflet::colorFactor(pals, domain=sites$site_colors)
			site_coords=sites[,c("MonitoringLocationIdentifier","MonitoringLocationName","MonitoringLocationTypeName","LatitudeMeasure","LongitudeMeasure","site_colors","MLID_DWQCat")]
			names(site_coords)[names(site_coords)=="MonitoringLocationIdentifier"]="locationID"
			names(site_coords)[names(site_coords)=="MonitoringLocationName"]="locationName"
			names(site_coords)[names(site_coords)=="MonitoringLocationTypeName"]="locationType"
			site_coords=sf::st_as_sf(site_coords, coords=c("LongitudeMeasure","LatitudeMeasure"), crs=4326, remove=F)
		}
		if(!missing(fac)){
			fac_coords=do.call(rbind.data.frame,fac$geometry$coordinates)
			names(fac_coords)=c("dec_long","dec_lat")
			fac_coords=data.frame(fac$properties[,c("SourceID","CWPName","CWPFacilityTypeIndicator")], (fac_coords))
			names(fac_coords)[names(fac_coords)=="SourceID"]="locationID"
			names(fac_coords)[names(fac_coords)=="CWPName"]="locationName"
			names(fac_coords)[names(fac_coords)=="CWPFacilityTypeIndicator"]="locationType"
			names(fac_coords)[names(fac_coords)=="dec_long"]="LongitudeMeasure"
			names(fac_coords)[names(fac_coords)=="dec_lat"]="LatitudeMeasure"
			fac_coords=sf::st_as_sf(fac_coords, coords=c("LongitudeMeasure","LatitudeMeasure"), crs=4326, remove=F)
		}
	
		if(exists('site_coords')){
			if(exists('fac_coords')){
				locs=rbind(site_coords,fac_coords)
				}else{locs=site_coords}
		}else{
			locs=fac_coords
			}
	
	
		#Color palette for points
		
		# pal <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
		# pal=leaflet::colorFactor(pal(length(unique(locs$locationType))), domain = locs$locationType)
		# 
		#Build map
		
		map=leaflet::leaflet(locs)
			map=leaflet::addProviderTiles(map, "Esri.WorldTopoMap", group = "Topo")
			map=leaflet::addProviderTiles(map,"Esri.WorldImagery", group = "Satellite")
			
			map=addMapPane(map,"underlay_polygons", zIndex = 410)
			map=addMapPane(map,"au_poly", zIndex = 415)
			map=addMapPane(map,"markers", zIndex = 420)

			map=leaflet::addCircleMarkers(map, lat=locs$LatitudeMeasure, lng=locs$LongitudeMeasure, group="Sites", 
				color = ~pal1(sites$site_colors), opacity=0.8, layerId=locs$locationID, options = pathOptions(pane = "markers"),
				popup = paste0(
					"Location ID: ", locs$locationID,
					"<br> Name: ", locs$locationName,
					"<br> Type: ", locs$locationType,
					"<br> Lat: ", locs$LatitudeMeasure,
					"<br> Long: ", locs$LongitudeMeasure,
				  "<br> Category: ", locs$MLID_DWQCat))
			map=leaflet::addCircles(map, lat=au_centroids$Y, lng=au_centroids$X, group="au_names", label=au_centroids$AU_NAME, stroke=F, fill=F,
				popup = paste0(
					"AU ID: ", au_centroids$ASSESS_ID,
					"<br> AU Name: ", au_centroids$AU_NAME,
					"<br> AU Type: ", au_centroids$AU_Type,
					"<br> Category: ", au_centroids$AU_DWQCat,
					"<br> New Impairment:", au_centroids$new_au_imp))
			map=leaflet::addCircles(map, lat=au_centroids$Y, lng=au_centroids$X, group="au_ids", label=au_centroids$ASSESS_ID, stroke=F, fill=F,
				popup = paste0(
					"AU name: ", au_centroids$AU_NAME,
					"<br> AU ID: ", au_centroids$ASSESS_ID,
					"<br> AU type: ", au_centroids$AU_Type,
					"<br> Category: ", au_centroids$AU_DWQCat,
					"<br> New Impairment:", au_centroids$new_au_imp))
			map=leaflet::addCircles(map, lat=locs$LatitudeMeasure, lng=locs$LongitudeMeasure, group="locationID", label=locs$locationID, stroke=F, fill=F,
				popup = paste0(
					"Location ID: ", locs$locationID,
					"<br> Name: ", locs$locationName,
					"<br> Type: ", locs$locationType,
					"<br> Lat: ", locs$LatitudeMeasure,
					"<br> Long: ", locs$LongitudeMeasure))
			map=leaflet::addCircles(map, lat=locs$LatitudeMeasure, lng=locs$LongitudeMeasure, group="locationName", label=locs$locationName, stroke=F, fill=F,
				popup = paste0(
					"Location ID: ", locs$locationID,
					"<br> Name: ", locs$locationName,
					"<br> Type: ", locs$locationType,
					"<br> Lat: ", locs$LatitudeMeasure,
					"<br> Long: ", locs$LongitudeMeasure))
			map=leaflet::addLabelOnlyMarkers(map, group="Labels", lat=locs$LatitudeMeasure, lng=locs$LongitudeMeasure,
				label=locs$locationID,labelOptions = leaflet::labelOptions(noHide = T, textsize = "15px"),
				clusterOptions=leaflet::markerClusterOptions(spiderfyOnMaxZoom=T))
			
			if(plot_polys){
				map=addPolygons(map, data=bu_poly,group="Beneficial uses",smoothFactor=4,fillOpacity = 0.1,weight=3,color="green", options = pathOptions(pane = "underlay_polygons"),
					popup=paste0(
						"Description: ", bu_poly$R317Descrp,
						"<br> Uses: ", bu_poly$bu_class)
					)
				map=addPolygons(map, data=au_poly,group="Assessment units",smoothFactor=2,fillOpacity = 0.5, layerId=au_poly$ASSESS_ID,weight=3,color=~pal1(au_poly$au_colors), options = pathOptions(pane = "au_poly"),
				                popup=paste0(
				                  "AU name: ", au_poly$AU_NAME,
				                  "<br> AU ID: ", au_poly$ASSESS_ID,
				                  "<br> AU type: ", au_poly$AU_Type,
				                  "<br> Category: ", au_poly$AU_DWQCat,
				                  "<br> New Impairment: ", au_poly$new_au_imp)
				)
				map=addPolygons(map, data=ss_poly,group="Site-specific standards",smoothFactor=4,fillOpacity = 0.1,weight=3,color="blue", options = pathOptions(pane = "underlay_polygons"),
					popup=paste0("SS std: ", ss_poly$SiteSpecif)
					)
				map=leaflet::addLayersControl(map,
					position ="topleft",
					baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites","Labels","Assessment units","Beneficial uses", "Site-specific standards"),
					options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))
				#map=hideGroup(map, "Assessment units")
				#map=hideGroup(map, "Sites")
				map=hideGroup(map, "Site-specific standards")
				map=hideGroup(map, "Beneficial uses")
				map=hideGroup(map, "Labels")
			}else{
				map=leaflet::addLayersControl(map,
					position ="topleft",
					baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites","Labels"),
					options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE))
				map=hideGroup(map, "Labels")
			}
			# map=leaflet::addLegend(map, position = 'topright',
			#                        colors = unique(pal1(au_poly$au_colors)), 
			#                        labels = unique(au_poly$au_colors))
			# #map=addControl(map, "<P><B>Search</B>", position='topleft')
			
			if("sites" %in% search & "aus" %in% search){
				map=addSearchFeatures(map,
					targetGroups = c('au_ids','au_names','locationID','locationName'),
					options = leaflet.extras::searchFeaturesOptions(
					zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
					autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))
			}
			if("sites" %in% search & !"aus" %in% search){
				map=addSearchFeatures(map,
					targetGroups = c('locationID','locationName'),
					options = leaflet.extras::searchFeaturesOptions(
					zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
					autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))
			}
			if(!"sites" %in% search & "aus" %in% search){
				map=addSearchFeatures(map,
					targetGroups = c('au_ids','au_names'),
					options = leaflet.extras::searchFeaturesOptions(
					zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
					autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))
			}
			
			map=leaflet::addMeasure(map, position="topright")
	}
	
return(map)

}

