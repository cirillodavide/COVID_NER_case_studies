library(visNetwork)
library(shinyjs)

server <- function(input, output, session){

	edges <- read.table('network_files/edges.tsv',sep='\t',header=T)
	nodes <- read.table('network_files/nodes.tsv',sep='\t',header=T)

	output$network  <- renderVisNetwork({
		visNetwork(nodes, edges) %>%
		visIgraphLayout(type='full', layout ='layout_with_kk') %>%
  		visOptions(highlightNearest = TRUE, selectedBy = list(variable="group", main="Label"))
  		#visPhysics(stabilization = FALSE) %>% visEdges(smooth = FALSE)
	})

}
