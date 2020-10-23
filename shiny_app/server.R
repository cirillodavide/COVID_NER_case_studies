library(visNetwork)
library(igraph)
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
		
		G <- graph_from_data_frame(edges, directed=FALSE, vertices=nodes$id)
		
		output$summary <- renderTable(cbind('num of edges'=as.integer(gsize(G)),
											'num of nodes'=as.integer(gorder(G)),
											'num of connected components'=as.integer(length(as.integer(gorder(G))))
									), caption = "Network summary", caption.placement = getOption("xtable.caption.placement", "top"))
		
		output$summary_comm <- renderTable(cbind('num of communities'=as.integer(length(unique(cluster_louvain(G)$membership)))))

		if(input$node_stats == "degree_centrality"){
				nodes$value <- degree(G)
			}
			if(input$node_stats == "page_rank"){
				nodes$value <- page_rank(G, directed = FALSE)$vector
			}
			if(input$node_stats == "betweenness_centrality"){
				nodes$value <- betweenness(G, v = V(G), directed = FALSE)
			}
			if(input$node_stats == "none"){
				nodes$value <- 1
			}

		list('nodes'=nodes,'edges'=edges)
	})

	observe({

		output$network  <- renderVisNetwork({
			lst <- pass_the_graph()
			nodes <- lst$nodes
			nodes$color <- cols[nodes$group]
			edges <- lst$edges
			visNetwork(nodes, edges) %>%
			visEdges(color = "rgba(84,84,84,0.5)", smooth = FALSE) %>%
			visIgraphLayout(type='full', layout ='layout_with_kk') %>%
	  		visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = T), selectedBy = list(variable="group", main="Highlight label"))
		})
		
		pass_the_graph <- eventReactive(c(input$plot,input$node_stats), {
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

		#Node ranking
		create_rank <- reactive({
			if(input$node_stats!='none'){	
	  			lst <- pass_the_graph()
				nodes <- lst$nodes
				tab <- nodes[order(nodes$value,decreasing=T),c('id','value')]
				if(input$node_stats == "degree_centrality"){
					tag <- "Degree centrality"
				}
				if(input$node_stats == "page_rank"){
					tag <- "PageRank"
				}
				if(input$node_stats == "betweenness_centrality"){
					tag <- "Betweenness centrality"
				}
				colnames(tab) <- c('Concept',tag)
				tab
			}
		})

	  	output$ranked_nodes <- renderTable({
	  		tab <- create_rank()
	  		head(tab,20)
	  	})

	  	output$download_rank <- downloadHandler(
    							filename = function() {
      								paste(input$node_stats, ".csv", sep = "")
    							},
    							content = function(file) {
      								write.csv(create_rank(), file, row.names = FALSE)
    							})

	  	#Communities
	  	detect_comm <- reactive({
	  		lst <- pass_the_graph()
			nodes <- lst$nodes
			edges <- lst$edges
			G <- graph_from_data_frame(edges, directed=FALSE, vertices=nodes$id)
			clusters <- cluster_louvain(G)
			data.frame('node'=nodes$id,'community'=clusters$membership)
	  	})

	  	output$network2  <- renderVisNetwork({
			lst <- pass_the_graph()
			nodes <- lst$nodes
			edges <- lst$edges
	  		nodes$group <- detect_comm()$community
			visNetwork(nodes, edges) %>%
			visEdges(color = "rgba(84,84,84,0.5)", smooth = FALSE) %>%
			visIgraphLayout(type='full', layout ='layout_with_kk') %>%
			visOptions(selectedBy = list(variable="group", main="Highlight comminities"))
	  	})

	  	output$download_comm <- downloadHandler(
    							filename = "communities.csv",
    							content = function(file) {
      								write.csv(detect_comm(), file, row.names = FALSE)
    							})

	  	# refresh button
	  	observeEvent(input$refresh, {
	    	session$reload()
	  	})

	})
}
