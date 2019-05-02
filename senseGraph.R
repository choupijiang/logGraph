require(shiny)
require(visNetwork)
require(jsonify)
data <-
  read.csv("~/Downloads/message-graph.csv",
           header = F,
           stringsAsFactors = F)
names(data) <- c("api", "from", "to", "value")
data$title <- data$value
apis <- unique(c(data$api))


ui <-    fluidPage(
  theme = "bootstrap.css",
  titlePanel("message graph of senseID log"),
  tags$head(
    # tags$script(src = 'https://code.jquery.com/jquery-1.11.1.min.js')
    includeScript("json-viewer/jquery.json-viewer.js"),
    includeCSS("json-viewer/jquery.json-viewer.css")
    # tags$script(src ="json-viewer/jquery.json-viewer.js"),
    # tags$link( href="json-viewer/jquery.json-viewer.css", rel="stylesheet")
    
  ),
  
  fluidRow(
    column(4,
           wellPanel(
             selectInput("request_path", "request_path", choices = apis),
             checkboxInput("schema", "show schema?", FALSE),
             hr(),
             
             helpText("Data from SenseId log.")
           ),
           HTML("<pre id='json-render'></pre>")
    ),
    
    column(8,
           visNetworkOutput("network", height = "600px"),
           verbatimTextOutput("sample")
          
    )
    
  ),
  HTML(
    "
    <script type='text/javascript'>
    $('#json-render').click(function(){
    try {
    var v = eval($('#sample').text());
    }
    catch (error) {
    return alert('Cannot eval JSON: ' + error);
    }
    var options = {
    collapsed: $('#collapsed').is(':checked'),
    rootCollapsable: $('#root-collapsable').is(':checked'),
    withQuotes: $('#with-quotes').is(':checked'),
    withLinks: $('#with-links').is(':checked')
    };
    $('#json-render').jsonViewer(v, {
    collapsed: false,
    rootCollapsable: false,
    withQuotes: false,
    withLinks: false
    });
    
    })
    </script>
    <style type='text/css'>
    #sample {
    visibility:hidden;
    }
    </style>"
  )
)

server <- function(input, output, session) {
  observe({
    myDataSet <- data[data$api == input$request_path,]
    messages <- unique(c(myDataSet$from, myDataSet$to))
    
    output$network <- renderVisNetwork({
      nodes <- data.frame(id = messages, label = messages)
      edges <- myDataSet
      visNetwork(nodes, edges, height = "100%", width = "100%") %>%
        visEdges(arrows = "to",
                 color = list(color = "lightblue", highlight = "red")) %>%
        visOptions(
          manipulation = TRUE,
          nodesIdSelection = TRUE,
          highlightNearest =  list(
            enabled = TRUE,
            degree = 2,
            hover = F
          )
        ) %>%
        visInteraction(hover = TRUE)  %>%
        visPhysics(stabilization = TRUE) %>%
        visEvents(hoverNode = "function(nodes) {
                  Shiny.onInputChange('current_node_id', nodes);
                  ;}")
  })
   
    output$sample <-  renderPrint({
      df <- data.frame(id = 1:2, val = rnorm(2))
      js <- to_json(df)
      # pretty_json(js)
      # input$current_node_id
      js
    })
    
  })
  }

shinyApp(ui, server)