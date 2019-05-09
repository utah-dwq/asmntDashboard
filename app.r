### Assessment Dashboard
### Version 2

# Packages
library(wqTools)
library(leaflet)
library(shinyBS)
library(irTools)
library(plotly)

#setwd('C:\\Users\\jvander\\Documents\\R\\asmntDashboard')
#site_use_param_asmnt=read.csv('data/site-use-param-asmnt.csv')

# Modules/functions
source('modules/initialDataProc.R')
source('modules/asmntMap.R')

# Load data & criteria
load('data/prepped_merged_data.Rdata')

options(warn = -1)

# User interface
ui <-fluidPage(
	# Header
	headerPanel(
		title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85), target="_blank"),
		tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="WQ Assessment Dashboard")
	),

	mainPanel(width=12,
		bsCollapse(multiple=T,
			bsCollapsePanel(list(icon('plus-circle'), icon('file-import'),"Import assessments file"), 
				fluidRow(
					column(2, fileInput("import_assessments", "Import assessment file", accept=".csv")),
					uiOutput('ex_url')
					#column(2, actionButton('example_input', icon=icon('question'), label='', style = "margin-top: 25px; color: #fff; background-color: #337ab7; border-color: #2e6da4%"))
				)
			),
			bsCollapsePanel(list(icon('plus-circle'), icon('map-marked-alt'),"Review map"),
				fluidRow(
					actionButton('clear_au', 'Clear selected AU(s)', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('trash-alt')),
					actionButton('build_tools', 'Build analysis tools', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('toolbox'))
					#column(1),
					#column(3, shinyWidgets::pickerInput("site_types","Site types to map:", choices=site_type_choices, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
					#column(3, uiOutput("review_reasons")),
					#column(3, uiOutput('ml_types')),
					#column(2, shinyWidgets::materialSwitch(inputId = "auto_zoom", label="Auto-zoom on", value = TRUE, right=T, status='primary'))
				),
				br(),
				
				# Map
				fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("assessment_map", height="600px"),size=2, color="#0080b7"))
			),
			bsCollapsePanel(list(icon('plus-circle'), icon('chart-bar'), "Figures"),
			                     tabsetPanel(
			                       tabPanel("Multi-Site Time Series",
			                                fluidRow(column(3,uiOutput("sel_comparameter",style = "margin-top: 25px")),
			                                         column(3,uiOutput("sel_compunit",style = "margin-top: 25px"))),
			                                fluidRow(column(3,radioButtons("compare_plottype", "Plot Type", choices = c("Time Series","Boxplots"), selected = "Time Series", inline = TRUE))),
			                                fluidRow(plotlyOutput("compare_sites"))),
			                       tabPanel("Single Site Time Series",
			                                fluidRow(column(3,uiOutput("sel_param_site",style = "margin-top: 25px"))),
			                                fluidRow(column(3,uiOutput("sel_param1")),
			                                         column(3,uiOutput("sel_param2"))),
			                                fluidRow(plotlyOutput("compare_params"))),
			                       tabPanel("Concentration Map",
			                                br(),
			                                fluidRow(br(),column(3,fluidRow(uiOutput("sel_maparameter", style = "margin-left: 25px")),
			                                                fluidRow(uiOutput("sel_paramdate", style = "margin-left: 25px"))),
			                                         column(9, shinycssloaders::withSpinner(leaflet::leafletOutput("conc_map", height="500px"),size=2, color="#0080b7")))
			                       )
			                     )
				# Figure inputs:
				#reactive_objects$sel_data
				#reactive_objects$sel_crit

			),
			bsCollapsePanel(list(icon('plus-circle'), icon('table'), "Data table"),
				fluidRow(downloadButton('exp_dt', label = "Export data table", icon='download', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%')),
				br(),
				fluidRow(div(DT::DTOutput("dt"), style = list("font-size:65%")))
			),
			bsCollapsePanel(list(icon('plus-circle'), icon('database'), "Download raw data from WQP"), 
				fluidRow(
					column(2, h4('Start date'), dateInput('StartDate', '', format='mm/dd/yyyy')),
					column(2, h4('End date'), dateInput('EndDate', '', format='mm/dd/yyyy'))
				),
				uiOutput('wqp_url')
				#actionButton('dwnld_wqp', 'Download WQP data', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('download'))
			                )
			)
	  )
)


# Server
server <- function(input, output, session){

options(warn=0)

# Example input url
output$ex_url <-renderUI(a(href='https://github.com/utah-dwq/asmntDashboard/blob/version2/data/site-use-param-asmnt.csv',list(icon('question'),"Example input data"),target="_blank"))

# Empty reactive objects
reactive_objects=reactiveValues()

# Import site-use-param-assessments file
observeEvent(input$import_assessments,{
	file=input$import_assessments$datapath
	site_use_param_asmnt=read.csv(file)
	inputs=initialDataProc(site_use_param_asmnt)
	reactive_objects$au_asmnt_poly=inputs$au_asmnt_poly
	reactive_objects$site_asmnt=inputs$site_asmnt
	reactive_objects$selected_aus=vector()
})

# Map output
output$assessment_map=leaflet::renderLeaflet({
	req(reactive_objects$au_asmnt_poly, reactive_objects$site_asmnt)
	asmntMap(reactive_objects$au_asmnt_poly, reactive_objects$site_asmnt)
})
asmnt_map_proxy=leafletProxy('assessment_map')

# Map polygon click to select AUs
observeEvent(input$assessment_map_shape_click,{
	au_click = input$assessment_map_shape_click$id
	if(!is.null(au_click)){
		if(au_click %in% reactive_objects$selected_aus){
			reactive_objects$selected_aus=reactive_objects$selected_aus[!reactive_objects$selected_aus %in% au_click]
		}else{
			reactive_objects$selected_aus=append(reactive_objects$selected_aus, au_click)
		}
	}
})

# Highlight AU polygon by adding new polygons via proxy
observeEvent(reactive_objects$selected_aus, ignoreNULL = F, ignoreInit=T, {
	req(reactive_objects$au_asmnt_poly)
	asmnt_map_proxy %>%
	clearGroup(group='highlight') %>%
	addPolygons(data=reactive_objects$au_asmnt_poly[reactive_objects$au_asmnt_poly$ASSESS_ID %in% reactive_objects$selected_aus,],
		group='highlight', options = pathOptions(pane = "highlight"), color='chartreuse', opacity = 0.75, fillOpacity = 0.4, weight = 10)
})

# Clear selected AUs with clear_au action button
observeEvent(input$clear_au, {
	reactive_objects$selected_aus=NULL
})

# Generate data and criteria subsets (based on selected AUs) for analysis tools on button press 
observeEvent(input$build_tools,{
	sel_sites=reactive_objects$site_asmnt$IR_MLID[reactive_objects$site_asmnt$ASSESS_ID %in% reactive_objects$selected_aus]
	reactive_objects$sel_sites=sel_sites
	reactive_objects$sel_data=subset(merged_data, IR_MLID %in% sel_sites)
	reactive_objects$sel_crit=subset(criteria, IR_MLID %in% sel_sites)
	showModal(modalDialog(title="Analysis tools ready.",size="l",easyClose=T,
		"Data and analysis tools ready. Scroll to 'Figures' and 'Data table' panels to review and plot data."))
})

# Figures

## Mult sites one param
#Create parameter selection drop down (from non duplicated dataset)
output$sel_comparameter <- renderUI({
    selectInput("sel_comparameter","Select Parameter", choices = c("",unique(reactive_objects$sel_data$R3172ParameterName)), selected = "")
})

output$sel_compunit <- renderUI({
  
  selectInput("sel_compunit","Select Unit", choices = c("",unique(reactive_objects$sel_data$R3172ParameterName)), selected = "")
})

output$compare_sites <- renderPlotly({
  req(input$sel_comparameter)

  plotdata = reactive_objects$sel_data
  plotdata = plotdata[order(plotdata$ActivityStartDate),]
  plotdata = plotdata[plotdata$R3172ParameterName==input$sel_comparameter,]

  if(is.na(plotdata$IR_Unit[1])){
    unit = ""
  }else{unit = plotdata$IR_Unit[1]}
  title = as.character(plotdata$R3172ParameterName[1])

  if(input$compare_plottype=="Time Series"){
    p = plot_ly(type = 'scatter', mode = 'lines+markers',x = plotdata$ActivityStartDate, y = plotdata$IR_Value, color = plotdata$IR_MLID, transforms = list(type = 'groupby', groups = plotdata$IR_MLID),
                marker = list(size=10))%>%
      layout(title = title,
             titlefont = list(
               family = "Arial, sans-serif"),
             font = list(
               family = "Arial, sans-serif"),
             xaxis = list(title = "Site"),
             yaxis = list(title = unit))
  }else{
    p = plot_ly(type = 'box', y = plotdata$IR_Value, color = plotdata$IR_MLID, transforms = list(type = 'groupby', groups = plotdata$IR_MLID))%>%
      layout(title = title,
             titlefont = list(
               family = "Arial, sans-serif"),
             font = list(
               family = "Arial, sans-serif"),
             xaxis = list(title = "Site"),
             yaxis = list(title = unit))
  }

})

##Mult params one site
# Site selection
  output$sel_param_site <- renderUI({
    param_site = as.character(unique(reactive_objects$sel_data$IR_MLID))
    selectInput("sel_param_site","Select Site", choices = c("",param_site), selected = "")
  })

# Parameter 1 selection based on site
  output$sel_param1 <- renderUI({
    reactive_objects$params_1 = unique(reactive_objects$sel_data$R3172ParameterName[reactive_objects$sel_data$IR_MLID==input$sel_param_site])
    selectInput("sel_param1", "Select Parameter 1", choices = c("",reactive_objects$params_1), selected = "")
  })
  
# Parameter 2 selection based on parameter 1
  output$sel_param2 <- renderUI({
    params_2 = reactive_objects$params_1[reactive_objects$params_1!=input$sel_param1]
    selectInput("sel_param2", "Select Parameter 2", choices = c("",params_2), selected = "")
  })

# Plot
output$compare_params <- renderPlotly({
  req(input$sel_param1)
  data = reactive_objects$sel_data
  plotdata = data[data$IR_MLID==input$sel_param_site&data$R3172ParameterName%in%c(input$sel_param1, input$sel_param2),]
  plotdata = plotdata[order(plotdata$ActivityStartDate),]
  param1 = plotdata[plotdata$R3172ParameterName==input$sel_param1,]
  param2 = plotdata[plotdata$R3172ParameterName==input$sel_param2,]

  p = plot_ly(type = 'scatter', mode = 'lines+markers')%>%
    layout(title = param1$IR_MLID[1],
           titlefont = list(
             family = "Arial, sans-serif"),
           font = list(
             family = "Arial, sans-serif"),
           yaxis = list(title = param1$IR_Unit[1]),
           yaxis2 = list(side="right", overlaying = "y",title = param2$IR_Unit[1]))%>%
    add_trace(x = param1$ActivityStartDate, y = param1$IR_Value, name = param1$R3172ParameterName[1], marker = list(size = 10))%>%
    add_trace(x = param2$ActivityStartDate, y = param2$IR_Value, name = param2$R3172ParameterName[1], marker = list(size = 10), yaxis = "y2")
})

##Spatial Relationships between site/params
output$sel_maparameter <- renderUI({
  selectInput("sel_maparameter","Select Parameter", choices = c("",unique(reactive_objects$sel_data$R3172ParameterName)), selected = "")
})

output$sel_paramdate <- renderUI({
  dates = as.character(unique(reactive_objects$sel_data$ActivityStartDate))
  dates = as.Date(dates, "%Y-%m-%d")
  sliderInput("sel_paramdate", "Select Date Range", min = min(dates), max = max(dates), value = c(min(dates),max(dates)))
})

# Map proxy
conc_proxy = leaflet::leafletProxy("conc_map")
# map
session$onFlushed(once = T, function() {
  output$conc_map <- leaflet::renderLeaflet({
    req(reactive_objects$sel_data)

    # Set map bounds
    data = reactive_objects$sel_data
    sites = unique(data[,c("IR_MLID","IR_Lat","IR_Long")])
    minlat = min(sites$IR_Lat)
    maxlat = max(sites$IR_Lat)
    minlong = min(sites$IR_Long)
    maxlong = max(sites$IR_Long)

    # Map parameters
    conc_map = wqTools::buildMap(plot_polys=TRUE, search="")
    conc_map = leaflet::addLayersControl(conc_map,
                                         position ="topleft",
                                         baseGroups = c("Topo","Satellite"),overlayGroups = c("Sites", "Assessment units","Beneficial uses", "Site-specific standards"),
                                         options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=TRUE))
    conc_map = fitBounds(conc_map, minlong, minlat, maxlong, maxlat)
    conc_map=addMapPane(conc_map,"site_markers", zIndex = 450)
    conc_map=hideGroup(conc_map, "Assessment units")
    conc_map=hideGroup(conc_map, "Site-specific standards")
    conc_map=hideGroup(conc_map, "Beneficial uses")
    conc_map=removeMeasure(conc_map)

    conc_map = addCircleMarkers(conc_map, data = sites, lat=sites$IR_Lat, lng=sites$IR_Long, layerId = sites$IR_MLID,group="Sites",
                                weight = 2, fill = TRUE, opacity=0.95, fillOpacity = 0.5, color = "green", radius = 5, options = pathOptions(pane = "site_markers"))

  })
})

 # observe inputs to change map proxy
observe({
req(input$sel_maparameter)
# Isolate data and map coordinates
if(!is.null(reactive_objects$sel_data)){
  data = reactive_objects$sel_data
  data$ActivityStartDate = as.Date(data$ActivityStartDate, "%Y-%m-%d")
  sites = unique(data[,c("IR_MLID","IR_Lat","IR_Long")])
  conc_sites = data[data$R3172ParameterName==input$sel_maparameter&data$ActivityStartDate>=input$sel_paramdate[1]&data$ActivityStartDate<=input$sel_paramdate[2],]
  avg_site_val = round(tapply(conc_sites$IR_Value, conc_sites$IR_MLID, mean),2)
  conc_radius = ((avg_site_val-mean(avg_site_val))/sd(avg_site_val)+3)*3
  conc_ncount = tapply(conc_sites$IR_Value, conc_sites$IR_MLID, length)
  conc_radii = data.frame(IR_MLID = names(conc_radius), Avg_IR_Value = avg_site_val, Radius = conc_radius, Ncount = conc_ncount)
  site_data = merge(sites, conc_radii)

  conc_proxy%>%clearMarkers()%>%addCircleMarkers(data = site_data, lat=site_data$IR_Lat, lng=site_data$IR_Long, layerId = site_data$IR_MLID,group="Sites",
                                                   weight = 2, fill = TRUE, opacity=0.95, fillOpacity = 0.5, radius = as.numeric(site_data$Radius), options = pathOptions(pane = "site_markers"),
                                                 popup = paste0(
                                                     "MLID: ", site_data$IR_MLID,
                                                     "<br> Average Parameter Value: ", site_data$Avg_IR_Value,
                                                     "<br> Sample Count: ", as.character(site_data$Ncount)))
  }

})



# Data table output
output$dt=DT::renderDT({
	req(reactive_objects$sel_data)
	DT::datatable(data.frame(lapply(reactive_objects$sel_data, as.factor)),
		selection='none', rownames=FALSE, filter="top",
		options = list(scrollY = '600px', paging = TRUE, scrollX=TRUE)
	)
})

# Export data table
output$exp_dt <- downloadHandler(
	filename=paste0('exported-data-', Sys.Date(),'.xlsx'),
	content = function(file) {writexl::write_xlsx(
		list(data=reactive_objects$sel_data),
		path = file, format_headers=F, col_names=T)}
)

# Download WQP data for sites
observe({
	if(!is.null(reactive_objects$sel_sites)){
		reactive_objects$wqp_url=wqTools::readWQP(start_date=input$StartDate, end_date=input$EndDate, type='result', siteid=reactive_objects$sel_sites, url_only=T)
	}
})
output$wqp_url <-renderUI(a(href=paste0(reactive_objects$wqp_url),"Download WQP data",target="_blank"))


}

## run app
shinyApp(ui = ui, server = server)




