library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(googlesheets4)
library(googledrive)
# authentication
library(shinyauthr)

# Authenticate with Google Sheets (follow googlesheets4 documentation for setup)
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "authconfig/.secrets"
)

# Replace with your Google Sheet ID
sheet_id <- drive_get("StMaryMahiber_2_18_2025")$id

# Define ui 
ui <- page_sidebar(
  fillable_mobile = TRUE,
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  title = "በዳላስ የማርያም ጽዋ ማህበርተኞች መረዳጃ ማህበር",
  sidebar = sidebar(
    title = "Expense Tracker:",
    dateInput("date", "Date", value = Sys.Date()),
    selectInput("category", "Category", choices = c("እንኳን ደህና መጡ", "ለቤት ኪራይ", "ኮሌጅ ተመራቂዎች", "አዲስ ተጋቢዎች", "ልጅ ሲወለድ", "የታመመ ለመጠየቅ", "ሌሎች")),
    textInput("description", "Description", value = ""),
    selectInput("payment", "Payment Method", choices = c("Cash", "Direct Check", "Zelle or CashApp", "Other"), selected = NULL),
    numericInput("amount", "Amount", value = 0),
    textAreaInput("notes", label = "Notes", placeholder = "Other Notes"),
    actionButton("add", "Add Expense")
  ),
 
  card(
    full_screen = FALSE,
    max_height = 650,
    h3("Expense Tracker Table", style = "text-align:center;"),  # Header Title
    DTOutput("expense_table")
  )
)

# Define Server Logic
server <- function(input, output, session) {
  # Reactive value to trigger data reload
  trigger <- reactiveVal(0)
  
  # Load data from Google Sheet
  expense_data <- reactive({
    trigger() # Depend on the trigger
    read_sheet(sheet_id)
  })
  
  # Render the editable table
  output$expense_table <- renderDT({
    datatable(expense_data(), editable = TRUE, 
    fillContainer = TRUE, 
     rownames = FALSE,
     options = list(lengthChange = FALSE, searching = FALSE))
  })
  
  # Observe edits in the table and update Google Sheet
  observeEvent(input$expense_table_cell_edit, {
    info <- input$expense_table_cell_edit
    updated_data <- expense_data()
    updated_data[info$row, info$col] <- info$value
    write_sheet(updated_data, sheet_id, sheet = 1)
    trigger(trigger() + 1) # Invalidate the reactive to reload data
  })
  
  # Add new expense
  observeEvent(input$add, {
    new_row <- data.frame(
      Date = as.character(format(input$date, "%m-%d-%Y")),
      Category = as.character(input$category),
      Description = input$description,
      `Payment Method` = as.character(input$payment),
      Amount = input$amount,
      Notes = as.character(input$notes)
    )
    # Append the new row to the Google Sheet
    sheet_append(sheet_id, new_row, sheet = 1)
    # Reset inputs
    updateDateInput(session, "date", value = Sys.Date())
    updateTextInput(session, "category", value = "")
    updateTextInput(session, "description", value = "")
    updateTextInput(session, "payment", value = "")
    updateNumericInput(session, "amount", value = 0)
    updateTextAreaInput(session, "notes", value = "")
    # Invalidate the reactive to reload data
    trigger(trigger() + 1)
  })
}

# Run the App
shinyApp(ui = ui, server = server)