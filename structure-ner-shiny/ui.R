library(visNetwork)
library(shinyjs)

lst <- c("PROCEDIMIENTO","SINTOMA","FARMACO","EDAD-SUJETO-ASISTENCIA","ENFERMEDAD","ENTIDAD-OBSERVABLE","FECHAS","PROTEINAS","SPECIES","TERRITORIO","PAIS","SEXO-SUJETO-ASISTENCIA","ID-SUJETO-ASISTENCIA","CALLE","HOSPITAL")
names(lst) <- lst

ui <- fluidPage(

		titlePanel(title=div(img(src="BSC_logo.png", height = 80, width = 80), "COVID-19 case-studies co-mention network")),
		
		sidebarLayout(position = "left",
			sidebarPanel(
				fluidRow(
					actionButton("plot", "Plot network"),
					actionButton("refresh", "Refresh session")
					),
				h2("Network visualization panel"),
				uiOutput("weight"),
				checkboxGroupInput("checkLabel", "Select labels:", choices = lst, selected = c(
					"SEXO-SUJETO-ASISTENCIA", "SINTOMA", "ENFERMEDAD", "PROTEINAS", "ENTIDAD-OBSERVABLE"))
				),

		mainPanel(
			tableOutput("summary"),
			visNetworkOutput("network"),
			br(),
			tabsetPanel(type = "tabs",
                tabPanel("Co-occurrence",
                  tableOutput("co_occurrences")),
                tabPanel("Node ranking", fluidRow(
                  	column(4,
                  		h4("Centrality measures"),
      					radioButtons("node_stats", "",
                    		choices = c("Degree centrality" = "degree_centrality",
                    		"Page Rank" = "page_rank",
                    		"Betweenness centrality" = "betweenness_centrality",
                    		"None" = "none"), selected = "betweenness_centrality")
      				),
      				column(8,
                  		br(),
                  		downloadLink("download_rank"),
                  		tableOutput("ranked_nodes")
                  	)
                )),
                tabPanel("Communities", fluidRow(
                  	br(),
                  	downloadLink("download_comm"),
                  	tableOutput("summary_comm"),
                  	visNetworkOutput("network2"))
                )
      		)
		)
	)
)