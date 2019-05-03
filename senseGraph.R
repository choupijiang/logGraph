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
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    } 
                    ")
     )
  ),
  headerPanel("MESSAGE GRAPH of senseID LOG"), 
  tags$head(
    # tags$script(src = 'https://code.jquery.com/jquery-1.11.1.min.js')
    includeScript("json-viewer/jquery.json-viewer.js"),
    includeCSS("json-viewer/jquery.json-viewer.css")
    # tags$script(src ="json-viewer/jquery.json-viewer.js"),
    # tags$link( href="json-viewer/jquery.json-viewer.css", rel="stylesheet")
    
  ),
  
  fluidRow(
    column(
      4,
      wellPanel(
        selectInput("request_path", "request_path", choices = apis),
        checkboxInput("schema", "show schema?", FALSE)
      ),
      hr(),
      conditionalPanel(
        condition = "input.schema == true",
        HTML("<b>schemas of this message</b>")
      ),
      conditionalPanel(
        condition = "input.schema == true",
        HTML(
          "
          <div style='height:500px;overflow:scroll;overflow-x:hidden;overflow-y:scroll;'>
          <pre id='json-render' style='height:500px;'></pre>
          </div>
          "
        )
      )
    ),
    
    column(
      8,
      visNetworkOutput("network", height = "800px"),
      verbatimTextOutput("sample")
    )
    
      ),
  HTML(
    "
    <script type='text/javascript'>
      $('#sample').bind('DOMSubtreeModified',function(){
        try {
          var v = eval($('#sample').text());
        }
        catch (error) {
          // return alert('Cannot eval JSON: ' + error +$('#sample').text());
        }
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
    myDataSet <- data[data$api == input$request_path, ]
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
      df <- data.frame(id = c("name"),
                       val = input$current_node_id$node)
      js <- to_json(df)
      js
    })
  })
}
shinyApp(ui, server)