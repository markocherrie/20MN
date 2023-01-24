# A text input
if(interactive()){
  library(shiny)
  library(shinyMobile)
  
  shinyApp(
    ui = f7Page(
      title = "My app",
      f7SingleLayout(
        navbar = f7Navbar(title = "f7Text"),
        f7Text(
          inputId = "caption",
          label = "Caption",
          value = "Data Summary",
          placeholder = "Your text here"
        ),
        verbatimTextOutput("value")
      )
    ),
    server = function(input, output) {
      output$value <- renderPrint({ input$caption })
    }
  )
}
# Update text input
if (interactive()) {
  library(shiny)
  library(shinyMobile)
  
  ui <- f7Page(
    f7SingleLayout(
      navbar = f7Navbar(title = "updateF7Text"),
      f7Block(f7Button("trigger", "Click me")),
      f7Text(
        inputId = "text",
        label = "",
        value = "Some text",
        placeholder = "Your text here"
      ),
      verbatimTextOutput("value")
    )
  )
  
  server <- function(input, output, session) {
    output$value <- renderPrint(input$text)
    observeEvent(input$trigger, {
      updateF7Text("text", value = "Updated Text")
    })
  }
  shinyApp(ui, server)
}