library(shiny)
library(DT)
library(rvest)

x<- data.frame(Item = NA,
               End_Date = NA,
               Brand_Name = NA,
               Source = NA,
               Link = NA,
               Item_Name = NA,
               Thumbnail = NA,
               Current_Price = NA,
               Delete = NA
)
x<-na.omit(x)
ui = shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        textInput("link","Link"),
        # Add button
        actionButton(inputId = "add.button", label = "Add", icon = 
                       icon("plus")), 
        # Delete button 
        actionButton(inputId = "delete.button", label = "Delete", icon = 
                       icon("minus"))
      ),
      mainPanel(
        dataTableOutput('table')
      )
    )
  )
)
server = function(input, output, session) {
  values <- reactiveValues()
  values$df <- x
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(1)
    
    inputs[len] <- as.character(FUN(paste0(id, counter$countervalue), ...))
    
    inputs
    print(inputs)
  }
  
  counter <- reactiveValues(countervalue = 0)
  
  observeEvent(input$add.button,{
    cat("addEntry\n")
    counter$countervalue <- counter$countervalue + 1
    print(counter$countervalue)
    #get page elements
    html <- read_html(input$link)
    
    end_date = html %>% html_elements("#endTime") %>% html_text2()
    
    source = "ZM"
    
    link = paste('<a href=', input$link, '>Link</a>', sep='')

    item_name = html %>% html_elements("#itemTitle") %>% html_text2()
    
    thumbnail = html %>% html_elements("#imgPreview") %>% html_attr("src")
    thumbnail = paste('<img src=', thumbnail, ' height=150><img>', sep='')
    thumbnail
    
    price = html %>% html_elements(".amount") %>% html_text2()
    price = price[1]
    
    Actions = shinyInput(actionButton, 1, 'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )
    
    newRow <- data.frame(counter$countervalue, end_date, NA, source, link, item_name, thumbnail, price, Actions)
    colnames(newRow)<-colnames(values$df)
    values$df <- rbind(values$df,newRow)
  })
  
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    print(selectedRow)
    values$df <- values$df[values$df$Item != selectedRow,]
  })
  
  output$table = renderDataTable({
    DT::datatable(values$df, escape = FALSE, rownames = FALSE)
  })
  
}
shinyApp(ui,server)