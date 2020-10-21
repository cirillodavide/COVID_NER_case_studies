library(visNetwork)
library(shinyjs)

ui <- fluidPage(
		titlePanel(title=div(img(src="BSC_logo.png", height = 80, width = 80), "COVID-19 case-studies co-mention network")),
		mainPanel(
			visNetworkOutput("network")
	)
)