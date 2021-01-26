#' title
#'
#' ui description
#'
#'
#' @import shiny 
#'
#'
#' @export

ui <-  shinydashboard::dashboardPage(

	header=shinydashboard::dashboardHeader(title = 'Catch locations', titleWidth = 325, 
							 tags$li(tags$img(src = 'noaa.png', title = 'NOAA', alt = 'NOAA', height = '50px', style = 'padding-top: 4px; padding-bottom: 4px; padding-right: 4px;'), class = 'dropdown')
	),

	sidebar=shinydashboard::dashboardSidebar(disable = T),
	
	body=shinydashboard::dashboardBody(
        
        ## Adjust map height
        #tags$style(type = 'text/css', '#Map { height: calc(100vh - 80px) !important; }'),
        tags$style(type = 'text/css', 'html, { height:100%; width:100% }'),
        ## App
        fluidRow(
            column(width = 3,
                   shinydashboard::tabBox(
                    width = NULL,
                    tabPanel('Catch',
                        selectInput(inputId="species",
                                    label = "select species",
                                    choices = list("Fish1","Fish2"),
                                    selected="Fish1"),
                        
                        numericInput(inputId="year_from",
                                       label=strong("Year To plot"),
                                       value=1975,
                                       min=1975,max=2017,step=1),
                        actionButton(inputId="plot_data","Plot"),
                        hr(),
                        
                        h4("EPU's (10 min Square)"),
                        checkboxInput(inputId='EPU_GB',label="GB",value=F),
                        checkboxInput(inputId='EPU_GOM',label="GoM",value=F),
                        checkboxInput(inputId='EPU_MAB',label="MAB",value=F),
                        checkboxInput(inputId='EPU_SS',label="SS",value=F),
                        hr(),
                        
                        
                        radioButtons('stat_areas', label = 'Management Areas', choices = list('Statistical Reporting Areas' = 'stat'), selected = 'stat'),
                        actionButton('stat_areas_add', 'Add', icon = icon('plus-circle')),
                        actionButton('stat_areas_remove', 'Remove', icon = icon('minus-circle')),
                        hr(),
                        p(tags$b('75m Depth Contour')),
                        actionButton('contour75m_add', 'Add', icon = icon('plus-circle')),
                        actionButton('contour75m_remove', 'Remove', icon = icon('minus-circle'))

    
                    )
                )  
            ),
            column(width = 9,
                   shinydashboard::box(title = 'Map', width = NULL, solidHeader = TRUE, status = 'primary', leaflet::leafletOutput('catch_map',width="100%",height="80vh"))
            )
        )
	)
)
    #ui <-  shinydashboard::dashboardPage(header, shinydashboard::dashboardSidebar(disable = TRUE), body)



#########################
## Create Shiny object ##
#########################

	#shinyApp(ui = ui, server = server)
 