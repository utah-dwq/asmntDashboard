# Figures module

#setwd('C:\\Users\\jvander\\Documents\\R\\asmntDashboard\\modules')
load('figures-test-data.Rdata')

ui <-fluidPage(
#figuresUI <- function(id){
	#ns <- NS(id)
#}
	
	fluidRow(
		column(2,fluidRow(uiOutput('sel_param1'), uiOutput('sel_units1'))),
		column(2,
			conditionalPanel("input.tabs=='Multiple parameters'",
				fluidRow(uiOutput('sel_param2'), uiOutput('sel_units2'))
			)
		)
	),
	tabsetPanel(id='tabs',
		tabPanel('Multiple sites',
			fluidRow(column(3,radioButtons("compare_plottype", "Plot Type", choices = c("Time Series","Boxplot", "Concentration Map"), selected = "Time Series", inline = TRUE))),
			plotlyOutput('multi_site')
			#fluidRow(plotlyOutput("compare_sites"))
		),
		tabPanel("Multiple parameters"
			#fluidRow(plotlyOutput("compare_params"))
		)
	)
)

server <- function(input, output, session){
	# Empty reactive objects
	reactive_objects=reactiveValues()
	
	
	# Make sure numeric criterion is numeric
	sel_crit$NumericCriterion=as.numeric(sel_crit$NumericCriterion)
	
	# Select param 1
	output$sel_param1 <- renderUI({
		selectInput("sel_param1","Select Parameter 1", choices = c(sel_data$R3172ParameterName))
	})
	
	# Select units 1
	output$sel_units1 <- renderUI({
		units=unique(sel_data[sel_data$R3172ParameterName == input$sel_param1, 'IR_Unit'])
		selectInput("sel_units1","Select Units 1", choices = units)
	})

	# Select param 2
	output$sel_param2 <- renderUI({
		param_choices=sel_data$R3172ParameterName[! sel_data$R3172ParameterName %in% input$sel_param1]
		selectInput("sel_param2","Select Parameter 2", choices = param_choices)
	})

	# Select units 2
	output$sel_units2 <- renderUI({
		units=unique(sel_data[sel_data$R3172ParameterName == input$sel_param2, 'IR_Unit'])
		selectInput("sel_units2","Select Units 2", choices = units)
	})

	# Generate parameter 1 data & criteria (need to do criteria still)
	observe({
		req(input$sel_param1, input$sel_units1)
		
		## Data
		param1=subset(sel_data, R3172ParameterName == input$sel_param1)
		### Convert units if multiple available
		if(length(unique(param1$IR_Unit)>1)){
			param1$target_unit=input$sel_units1
			param1=wqTools::convertUnits(param1, input_units='IR_Unit', target_units = "target_unit", value_var='IR_Value', conv_val_col='plot_value')
		}else{param1$plot_value=param1$IR_Value}
		reactive_objects$param1=unique(param1[,c('IR_MLID','ActivityStartDate','IR_Lat','IR_Long','R3172ParameterName','plot_value','target_unit','IR_MLNAME','IR_DetCond','IR_Fraction','ASSESS_ID','AU_NAME','AU_Type','BEN_CLASS')])
		#param1<<-param1
		
		## Criteria
		crit1=subset(sel_crit, R3172ParameterName == input$sel_param1)
		### Convert units if multiple available
		if(length(unique(crit1$CriterionUnits)>1)){
			crit1$target_unit=input$sel_units1
			crit1=wqTools::convertUnits(crit1, input_units='CriterionUnits', target_units = "target_unit", value_var='NumericCriterion', conv_val_col='plot_value')
		}else{crit1$plot_value=crit1$NumericCriterion}
		#crit1<<-crit1
		reactive_objects$crit1<-crit1
		
	})

	# Generate parameter 2 data & criteria (need to do criteria still)
	observe({if(input$tabs=='Multiple parameters'){
		req(input$sel_param2, input$sel_units2)
		param2=subset(sel_data, R3172ParameterName == input$sel_param2)
		## Convert units if multiple available
		if(length(unique(param2$IR_Unit)>1)){
			param2$target_unit=input$sel_units2
			param2=wqTools::convertUnits(param2, input_units='IR_Unit', target_units = "target_unit", value_var='IR_Value', conv_val_col='plot_value')
		}else{param2$plot_value=param2$IR_Value}
		reactive_objects$param2=unique(param2[,c('IR_MLID','ActivityStartDate','IR_Lat','IR_Long','R3172ParameterName','plot_value','target_unit','IR_MLNAME','IR_DetCond','IR_Fraction','ASSESS_ID','AU_NAME','AU_Type','BEN_CLASS')])
	}})	
	

observe({
	req(reactive_objects$param1, reactive_objects$crit1)
	title = input$sel_param1
	ylab = paste0(input$sel_param1,' (', input$sel_units1,')')
	mlid_len=length(unique(reactive_objects$param1$IR_MLID))
	au_len=length(unique(reactive_objects$param1$ASSESS_ID))
	mlid_vis=as.list(append(rep(T,mlid_len), rep(F, au_len)))
	au_vis=as.list(append(rep(F,mlid_len), rep(T, au_len)))
	
	suppressWarnings({
		if(input$compare_plottype=="Time Series"){
			output$multi_site=renderPlotly({
				plot_ly(type = 'scatter', mode = 'lines+markers', x=as.Date(reactive_objects$param1$ActivityStartDate), y = reactive_objects$param1$plot_value, color = reactive_objects$param1$IR_MLID, marker = list(size=10), visible=T) %>%
					add_trace(type='scatter', mode = 'markers',x = as.Date(reactive_objects$param1$ActivityStartDate), y=reactive_objects$param1$plot_value, color = reactive_objects$param1$ASSESS_ID, marker = list(size=10), visible=F) %>%
						layout(title = title,
								titlefont = list(
								family = "Arial, sans-serif"),
								font = list(
								family = "Arial, sans-serif"),
								xaxis = list(title = "Date"),
								yaxis = list(title = ylab),
							updatemenus = list(
								list(
									buttons = list(
										list(method = "update", label='Group by site', 
											args = list(list(visible = mlid_vis))
										),
										list(method = "update", label='Group by AU', 
											args = list(list(visible = au_vis))
										)
									)
								)
							)
						) %>% 
					config(displaylogo = FALSE, collaborate = FALSE,
						modeBarButtonsToRemove = c(
							'sendDataToCloud',
							'hoverClosestCartesian',
							'hoverCompareCartesian',
							'lasso2d',
							'select2d'
						)
					)
	
			})
		}
		if(input$compare_plottype=="Boxplot"){
			output$multi_site=renderPlotly({
				plot_ly(data=reactive_objects$param1, type = 'box', y = ~plot_value, color = ~IR_MLID, visible=T) %>%
					add_trace(type = 'box', y = reactive_objects$param1$plot_value, color = reactive_objects$param1$ASSESS_ID, visible=F) %>%
					layout(title = title,
						titlefont = list(
						family = "Arial, sans-serif"),
						font = list(
						family = "Arial, sans-serif"),
						xaxis = list(title = "Site"),
						yaxis = list(title = ylab),
						updatemenus = list(
							list(
								buttons = list(
									list(method = "update", label='Group by site', 
										args = list(list(visible = mlid_vis))
									),
									list(method = "update", label='Group by AU', 
										args = list(list(visible = au_vis))
									)
								)
							)
						)
					) %>% 
					config(displaylogo = FALSE, collaborate = FALSE,
						modeBarButtonsToRemove = c(
							'sendDataToCloud',
							'hoverClosestCartesian',
							'hoverCompareCartesian',
							'lasso2d',
							'select2d'
						)
					)
			})
		}
		if(input$compare_plottype=="Concentration Map"){
		}
	})
})


	
#output$compare_sites <- renderPlotly({
#	req(reactive_objects$param1, reactive_objects$crit1)
#	title = input$sel_param1
#	if(input$compare_plottype=="Time Series"){
#		p = plot_ly(type = 'scatter', mode = 'lines+markers',x = reactive_objects$param1$ActivityStartDate, y = reactive_objects$param1$Plot_Value, color = reactive_objects$param1$IR_MLID, transforms = list(type = 'groupby', groups = reactive_objects$param1$IR_MLID),
#					marker = list(size=10))%>%
#		layout(title = title,
#				titlefont = list(
#				family = "Arial, sans-serif"),
#				font = list(
#				family = "Arial, sans-serif"),
#				xaxis = list(title = "Site"),
#				yaxis = list(title = reactive_objects$param1$target_unit[1]))
#	}#else{
#	p = plot_ly(type = 'box', y = plotdata$Plot_Value, color = plotdata$IR_MLID, transforms = list(type = 'groupby', groups = plotdata$IR_MLID))%>%
#	layout(title = title,
#			titlefont = list(
#			family = "Arial, sans-serif"),
#			font = list(
#			family = "Arial, sans-serif"),
#			xaxis = list(title = "Site"),
#			yaxis = list(title = plotdata$target_unit[1]))
#}
#
#})
#	
#	##Mult params one site
#	# Site selection
#	output$sel_param_site <- renderUI({
#		param_site = as.character(unique(reactive_objects$sel_data$IR_MLID))
#		selectInput("sel_param_site","Select Site", choices = c("",param_site), selected = "")
#	})
#	
#	# Parameter 1 selection based on site
#	output$sel_param1 <- renderUI({
#		reactive_objects$params_1 = unique(reactive_objects$sel_data$R3172ParameterName[reactive_objects$sel_data$IR_MLID==input$sel_param_site])
#		selectInput("sel_param1", "Select Parameter 1", choices = c("",reactive_objects$params_1), selected = "")
#	})
#	
#	# Parameter 2 selection based on parameter 1
#	output$sel_param2 <- renderUI({
#		params_2 = reactive_objects$params_1[reactive_objects$params_1!=input$sel_param1]
#		selectInput("sel_param2", "Select Parameter 2", choices = c("",params_2), selected = "")
#	})
#	
#	# Plot
#	output$compare_params <- renderPlotly({
#	req(input$sel_param1)
#	data = reactive_objects$sel_data
#	plotdata = data[data$IR_MLID==input$sel_param_site&data$R3172ParameterName%in%c(input$sel_param1, input$sel_param2),]
#	plotdata = plotdata[order(plotdata$ActivityStartDate),]
#	param1 = plotdata[plotdata$R3172ParameterName==input$sel_param1,]
#	param2 = plotdata[plotdata$R3172ParameterName==input$sel_param2,]
#	
#	p = plot_ly(type = 'scatter', mode = 'lines+markers')%>%
#		layout(title = param1$IR_MLID[1],
#			titlefont = list(
#				family = "Arial, sans-serif"),
#			font = list(
#				family = "Arial, sans-serif"),
#			yaxis = list(title = param1$IR_Unit[1]),
#			yaxis2 = list(side="right", overlaying = "y",title = param2$IR_Unit[1]))%>%
#		add_trace(x = param1$ActivityStartDate, y = param1$IR_Value, name = param1$R3172ParameterName[1], marker = list(size = 10))%>%
#		add_trace(x = param2$ActivityStartDate, y = param2$IR_Value, name = param2$R3172ParameterName[1], marker = list(size = 10), yaxis = "y2")
#	})
#	
#	##Spatial Relationships between site/params
#	output$sel_maparameter <- renderUI({
#	selectInput("sel_maparameter","Select Parameter", choices = c("",unique(reactive_objects$sel_data$R3172ParameterName)), selected = "")
#	})
#	
#	output$sel_paramdate <- renderUI({
#	dates = as.character(unique(reactive_objects$sel_data$ActivityStartDate))
#	dates = as.Date(dates, "%Y-%m-%d")
#	sliderInput("sel_paramdate", "Select Date Range", min = min(dates), max = max(dates), value = c(min(dates),max(dates)))
#	})
#	
#	# Map proxy
#	
#	
#	
#	
#	
#	
#	
#	
#	conc_proxy = leaflet::leafletProxy("conc_map")
#	# map
#	session$onFlushed(once = T, function() {
#	output$conc_map <- leaflet::renderLeaflet({
#		req(reactive_objects$sel_data)
#	
#		# Set map bounds
#		data = reactive_objects$sel_data
#		sites = unique(data[,c("IR_MLID","IR_Lat","IR_Long")])
#		minlat = min(sites$IR_Lat)
#		maxlat = max(sites$IR_Lat)
#		minlong = min(sites$IR_Long)
#		maxlong = max(sites$IR_Long)
#	
#		# Map parameters
#		conc_map = wqTools::buildMap(plot_polys=TRUE, search="")
#		conc_map = leaflet::addLayersControl(conc_map,
#											position ="topleft",
#											baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites", "Assessment units","Beneficial uses", "Site-specific standards"),
#											options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=TRUE))
#		conc_map = fitBounds(conc_map, minlong, minlat, maxlong, maxlat)
#		conc_map=addMapPane(conc_map,"site_markers", zIndex = 450)
#		conc_map=hideGroup(conc_map, "Assessment units")
#		conc_map=hideGroup(conc_map, "Site-specific standards")
#		conc_map=hideGroup(conc_map, "Beneficial uses")
#		conc_map=removeMeasure(conc_map)
#	
#		conc_map = addCircleMarkers(conc_map, data = sites, lat=sites$IR_Lat, lng=sites$IR_Long, layerId = sites$IR_MLID,group="Sites",
#									weight = 2, fill = TRUE, opacity=0.95, fillOpacity = 0.5, color = "green", radius = 5, options = pathOptions(pane = "site_markers"))
#	
#	})
#	})
#	
#	# observe inputs to change map proxy
#	observe({
#	req(input$sel_maparameter)
#	# Isolate data and map coordinates
#	if(!is.null(reactive_objects$sel_data)){
#	data = reactive_objects$sel_data
#	data$ActivityStartDate = as.Date(data$ActivityStartDate, "%Y-%m-%d")
#	sites = unique(data[,c("IR_MLID","IR_Lat","IR_Long")])
#	conc_sites = data[data$R3172ParameterName==input$sel_maparameter&data$ActivityStartDate>=input$sel_paramdate[1]&data$ActivityStartDate<=input$sel_paramdate[2],]
#	avg_site_val = round(tapply(conc_sites$IR_Value, conc_sites$IR_MLID, mean),2)
#	conc_radius = ((avg_site_val-mean(avg_site_val))/sd(avg_site_val)+3)*3
#	conc_ncount = tapply(conc_sites$IR_Value, conc_sites$IR_MLID, length)
#	conc_radii = data.frame(IR_MLID = names(conc_radius), Avg_IR_Value = avg_site_val, Radius = conc_radius, Ncount = conc_ncount)
#	site_data = merge(sites, conc_radii)
#	
#	conc_proxy%>%clearMarkers()%>%addCircleMarkers(data = site_data, lat=site_data$IR_Lat, lng=site_data$IR_Long, layerId = site_data$IR_MLID,group="Sites",
#													weight = 2, fill = TRUE, opacity=0.95, fillOpacity = 0.5, radius = as.numeric(site_data$Radius), options = pathOptions(pane = "site_markers"),
#													popup = paste0(
#														"MLID: ", site_data$IR_MLID,
#														"<br> Average Parameter Value: ", site_data$Avg_IR_Value,
#														"<br> Sample Count: ", as.character(site_data$Ncount)))
#	}
#	
#	})
}



## run app
shinyApp(ui = ui, server = server)

