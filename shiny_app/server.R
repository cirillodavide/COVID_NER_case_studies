library(visNetwork)
library(shinyjs)
library(viridis)

edges <- read.table('network_files/edges.tsv',sep='\t',header=T)
nodes <- read.table('network_files/nodes.tsv',sep='\t',header=T)

set.seed(123)
n <- length(levels(nodes$group))
cols <- sample(plasma(n))
names(cols) <- levels(nodes$group)

server <- function(input, output, session){

	selected_graph <- reactive({
		req(input$weight)
		req(input$checkLabel)
		edges <- edges[edges$value>input$weight,]
		nodes <- nodes[nodes$group%in%input$checkLabel,]
		edges <- edges[edges$from%in%nodes$id & edges$to%in%nodes$id,]
		nodes <- nodes[nodes$id%in%unique(c(as.character(unique(edges$from)),as.character(unique(edges$to)))),]
		list('nodes'=nodes,'edges'=edges)
	})

	output$network  <- renderVisNetwork({
		lst <- plot()
		nodes <- lst$nodes
		nodes$color <- cols[nodes$group]
		edges <- lst$edges
		visNetwork(nodes, edges) %>%
		visEdges(color = "rgba(84,84,84,0.5)", smooth = FALSE) %>%
		visIgraphLayout(type='full', layout ='layout_with_kk') %>%
  		visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = T), selectedBy = list(variable="group", main="Label"))
	})
	
	plot <- eventReactive(input$plot, {
		return(selected_graph())
	})

	#slider edge weight
  	output$weight <- renderUI({
  		sliderInput("weight",
                  	"Select the minimum number of co-mentioning documents:",
                  	min = 1,
                  	max = 20,
				  	value = 15,
				  	step = 1)
	})

}
