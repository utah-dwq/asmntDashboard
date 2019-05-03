### Assessment Dashboard
### Version 2

# Packages
library(wqTools)
library(leaflet)
library(shinyBS)
library(irTools)

#setwd('C:\\Users\\jvander\\Documents\\R\\asmntDashboard')
#site_use_param_asmnt=read.csv('data/site-use-param-asmnt.csv')

# Modules
source('modules/initialDataProc.R')
source('modules/asmntMap.R')


options(warn = -1)

# User interface
ui <-fluidPage(
# Header
headerPanel(
	title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85), target="_blank"),
	tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="WQ Assessment Dashboard")
),

mainPanel(width=12,
	bsCollapse(multiple=T, open='Import assessments',
		bsCollapsePanel("Import assessments", 
			fluidRow(
				column(2, fileInput("import_assessments", "Import assessment file", accept=".csv")),
				column(2, actionButton('example_input', icon=icon('question'), label='', style = "margin-top: 25px;"))
			)
		),
		bsCollapsePanel(list(icon('map-marked-alt'),"Review map"),
			fluidRow(
				column(1),
				#column(3, shinyWidgets::pickerInput("site_types","Site types to map:", choices=site_type_choices, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
				#column(3, uiOutput("review_reasons")),
				#column(3, uiOutput('ml_types')),
				column(2, shinyWidgets::materialSwitch(inputId = "auto_zoom", label="Auto-zoom on", value = TRUE, right=T, status='primary'))
			),
	
			# Map
			fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("assessment_map", height="600px"),size=2, color="#0080b7"))
		)
	)
)
)


# Server
server <- function(input, output, session){

options(warn=0)

# Empty reactive objects
reactive_objects=reactiveValues()

# Import site-use-param-assessments file
observeEvent(input$import_assessments,{
	file=input$import_assessments$datapath
	site_use_param_asmnt=read.csv(file)
	inputs=initialDataProc(site_use_param_asmnt)
	reactive_objects$au_asmnt_poly=inputs$au_asmnt_poly
	reactive_objects$site_asmnt=inputs$site_asmnt
})

# Map output
output$assessment_map=leaflet::renderLeaflet({
	asmntMap(reactive_objects$au_asmnt_poly, reactive_objects$site_asmnt)
})
asmnt_map_proxy=leafletProxy('assessment_map')

# Map polygon click to select an AU
observe({
	sel_au = input$assessment_map_shape_click$id
	print(sel_au)
})

# Map site click
observe({
	sel_site = input$assessment_map_marker_click$id
	print(sel_site)
})


}

## run app
shinyApp(ui = ui, server = server)




