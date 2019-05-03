asmntMap=function(au_asmnt_poly, site_asmnt){
	ss_poly=wqTools::ss_poly
	bu_poly=wqTools::bu_poly
	assessment_map <- 
		buildMap(plot_polys = F) %>%
			leaflet::addCircleMarkers(data=site_asmnt, lat=~IR_Lat, lng=~IR_Long, group="Sites",
				color = ~col, opacity=0.8, layerId=~IR_MLID, options = pathOptions(pane = "markers"),
				popup = paste0(
					"IR MLID: ", site_asmnt$IR_MLID,
					"<br> IR MLNAME: ", site_asmnt$IR_MLNAME,
					"<br> Assessment: ", site_asmnt$AssessCat,
					"<br> Impaired params: ", site_asmnt$Impaired_params,
					"<br> ID w/ exceedance params: ", site_asmnt$idE_params)
			) %>%	
			addPolygons(data=bu_poly,group="Beneficial uses",smoothFactor=4,fillOpacity = 0.1,weight=3,color="green", options = pathOptions(pane = "underlay_polygons"),
				popup=paste0(
					"Description: ", bu_poly$R317Descrp,
					"<br> Uses: ", bu_poly$bu_class)
			) %>%
			addPolygons(data=au_asmnt_poly,group="Assessment units",smoothFactor=4,fillOpacity = 0.1, layerId=~ASSESS_ID, weight=3,color=~col, options = pathOptions(pane = "au_poly"),
				popup=paste0(
					"AU name: ", au_asmnt_poly$AU_NAME,
					"<br> AU ID: ", au_asmnt_poly$ASSESS_ID,
					"<br> Assessment: ", au_asmnt_poly$AssessCat,
					"<br> Impaired params: ", au_asmnt_poly$Impaired_params,
					"<br> ID w/ exceedance params: ", au_asmnt_poly$idE_params)
			) %>%
			addPolygons(data=ss_poly,group="Site-specific standards",smoothFactor=4,fillOpacity = 0.1,weight=3,color="blue", options = pathOptions(pane = "underlay_polygons"),
				popup=paste0("SS std: ", ss_poly$SiteSpecif)
			) %>%
			leaflet::addLayersControl(position ="topleft",
				baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites", "Assessment units","Beneficial uses", "Site-specific standards"),
				options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE)) %>%
			#hideGroup("Assessment units") %>%
			hideGroup("Site-specific standards") %>%
			hideGroup("Beneficial uses") %>%
			hideGroup("Sites") %>%
			leaflet::addLegend(position = 'topright',
						colors = c('green','yellow','orange','red','grey'), 
						labels = c('Fully supporting', 'Insufficient data, no exceedances', 'Insufficient data, exceedances', 'Not supporting', 'Not assessed'))
	
return(assessment_map)	
	
}
	