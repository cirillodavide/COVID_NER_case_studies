library(visNetwork)
library(shinyjs)

lst <- c("PROCEDIMIENTO","SINTOMA","FARMACO","EDAD-SUJETO-ASISTENCIA","ENFERMEDAD","ENTIDAD-OBSERVABLE","FECHAS","PROTEINAS","SPECIES","TERRITORIO","PAIS","SEXO-SUJETO-ASISTENCIA","ID-SUJETO-ASISTENCIA","CALLE","HOSPITAL")
names(lst) <- lst

ui <- fluidPage(

		titlePanel(title=div(img(src="BSC_logo.png", height = 80, width = 80), "COVID-19 case-studies co-mention network")),
		
		sidebarLayout(position = "left",
			sidebarPanel(
				uiOutput("weight"),
				checkboxGroupInput("checkLabel", "Select labels:", choices = lst, selected = c(
					"SEXO-SUJETO-ASISTENCIA", "SINTOMA", "ENFERMEDAD", "PROTEINAS", "ENTIDAD-OBSERVABLE"))
				),

		mainPanel(
			actionButton("plot", "Plot network"),
			visNetworkOutput("network")
		)
	)
)