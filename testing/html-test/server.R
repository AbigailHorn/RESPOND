# shinyServer(function(input, output) {
#   getPage<-function() {
#     return(includeHTML("include.html"))
#   }
#   output$inc<-renderUI({getPage()})
# })

shinyServer(function(input, output) {
  getPage<-function() {
    if (input$html == "Choose site") {
      return(includeHTML("fragment.html"))}
    if (input$html == "Georgia") {
      return(includeHTML("Georgia.html"))}
    if (input$html == "LA County") {
      return(includeHTML("LAC.html"))}
    if (input$html == "Fragment") {
      return(includeHTML("fragment.html"))}
  }
  output$inc<-renderUI({getPage()})
  output$text <- renderText(input$html)
})

# shinyServer(function(input, output, session) {
#   output$numeric <- renderUI({
#     if (input$type == "slider") {
#       sliderInput("dynamic", input$label, value = 0, min = 0, max = 10)
#     } else {
#       numericInput("dynamic", input$label, value = 0, min = 0, max = 10) 
#     }
#   })
# })