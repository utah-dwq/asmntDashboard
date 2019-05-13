# Figures module

#setwd('C:\\Users\\jvander\\Documents\\R\\asmntDashboard\\modules')
#load('figures-test-data.Rdata')

figuresModUI <- function(id){
	ns <- NS(id)
tagList(	
	fluidRow(
		column(2,fluidRow(uiOutput(ns('sel_param1')), uiOutput(ns('sel_units1')))),
		column(2,
			conditionalPanel(paste0("input['", ns("tabs"),"'] == 'Multiple parameters' "),
				fluidRow(uiOutput(ns('sel_param2')), uiOutput(ns('sel_units2')))
			)
		)
	),
	tabsetPanel(id=ns('tabs'),
		tabPanel('Multiple sites',
			fluidRow(column(3,radioButtons(ns("compare_plottype"), "Plot Type", choices = c("Time Series","Boxplot", "Concentration Map"), selected = "Time Series", inline = TRUE))),
			plotlyOutput(ns('multi_site'))
			#fluidRow(plotlyOutput("compare_sites"))
		),
		tabPanel("Multiple parameters"
			#fluidRow(plotlyOutput("compare_params"))
		)
	)
)
}


figuresMod <- function(input, output, session, sel_data, sel_crit){

	# Empty reactive objects
	reactive_objects=reactiveValues()
	
	
	# Make sure numeric criterion is numeric
	sel_crit$NumericCriterion=as.numeric(sel_crit$NumericCriterion)
	
	# Select param 1
	output$sel_param1 <- renderUI({
		ns <- session$ns
		selectInput(ns("sel_param1"),"Select Parameter 1", choices = c(sel_data$R3172ParameterName))
	})
	
	# Select units 1
	output$sel_units1 <- renderUI({
		ns <- session$ns
		units=unique(sel_data[sel_data$R3172ParameterName == input$sel_param1, 'IR_Unit'])
		selectInput(ns("sel_units1"),"Select Units 1", choices = units)
	})

	# Select param 2
	output$sel_param2 <- renderUI({
		ns <- session$ns
		param_choices=sel_data$R3172ParameterName[! sel_data$R3172ParameterName %in% input$sel_param1]
		selectInput(ns("sel_param2"),"Select Parameter 2", choices = param_choices)
	})

	# Select units 2
	output$sel_units2 <- renderUI({
		ns <- session$ns
		units=unique(sel_data[sel_data$R3172ParameterName == input$sel_param2, 'IR_Unit'])
		selectInput(ns("sel_units2"),"Select Units 2", choices = units)
	})

	# Generate parameter 1 data & criteria
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
	

	# Figures outputs
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

}


	
