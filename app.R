library(shiny)
library(bslib)
library(DT)
library(googlesheets4)
library(googledrive)
library(shinyauthr)
library(shinyjs)
library(sodium)

# Authentication setup
options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "authconfig/.secrets"
)

options(gargle_oauth_email = TRUE)
gs4_auth(path = "stmaryr-0b787a4ad0dc.json")
drive_auth(path = "stmaryr-0b787a4ad0dc.json")
# user based system Environment variable
# user_base_loaded <- jsonlite::fromJSON(Sys.getenv("USER_BASE"))
user_base <- tibble::tibble(
  user = c("yihun", "metalem"),
  password = c(sodium::password_store("yihun21"), sodium::password_store("metalem21")),
  permissions = c("admin", "standard"),
  name = c("Yihun Zeleke", "Metalem Yazachew")
)

# sheet_id <- drive_get("StMaryMahiber")$id
sheet_id <- "14IlZf_5dPNwVrDPUjV68Rp6p-a4pd11mytAEtkiDAD0"

# Modified UI with proper hidden wrapper
ui <- tagList(
  shinyjs::useShinyjs(),
  shinyauthr::loginUI("login"),
  
  shinyjs::hidden(
    div(  # Main wrapper div
      id = "main_content",
      page_sidebar(
        fillable_mobile = TRUE,
        theme = bs_theme(
          version = 5,
          bootswatch = "minty"
        ),
        title = div(
          "በዳላስ የማርያም ጽዋ ማህበርተኞች መረዳጃ ማህበር",
          div(
            style = "position: absolute; right: 20px; top: 8px;",
            shinyauthr::logoutUI("logout", label = "Log Out")
          )
        ),
        sidebar = sidebar(
          title = "Expense Tracker:",
          dateInput("date", "Date", value = Sys.Date()),
          selectInput("category", "Category", choices = c("እንኳን ደህና መጡ", "ለቤት ኪራይ", "ኮሌጅ ተመራቂዎች", "አዲስ ተጋቢዎች", "ልጅ ሲወለድ", "የታመመ ለመጠየቅ", "ሌሎች")),
          textInput("description", "Description", value = ""),
          selectInput("payment", "Payment Method", choices = c("Cash", "Direct Check", "Zelle or CashApp", "Other")),
          numericInput("amount", "Amount", value = 0),
          textAreaInput("notes", label = "Notes", placeholder = "Other Notes"),
          actionButton("add", "Add Expense")
        ),
        card(
          full_screen = FALSE,
          max_height = 650,
          h3("Expense Tracker Table", style = "text-align:center;"),
          DTOutput("expense_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  observeEvent(credentials()$user_auth, {
    shinyjs::toggle("main_content", condition = credentials()$user_auth)
    shinyjs::toggle("login", condition = !credentials()$user_auth)
  })
  
  trigger <- reactiveVal(0)
  
  expense_data <- reactive({
    req(credentials()$user_auth)
    trigger()
    read_sheet(sheet_id)
  })
  
  output$expense_table <- renderDT({
    req(credentials()$user_auth)
    datatable(expense_data(), 
              editable = TRUE, 
              fillContainer = TRUE, 
              rownames = FALSE,
              options = list(lengthChange = FALSE, searching = FALSE))
  })
  
  observeEvent(input$expense_table_cell_edit, {
    req(credentials()$user_auth)
    info <- input$expense_table_cell_edit
    updated_data <- expense_data()
    updated_data[info$row, info$col] <- info$value
    write_sheet(updated_data, sheet_id, sheet = 1)
    trigger(trigger() + 1)
  })
  
  observeEvent(input$add, {
    req(credentials()$user_auth)
    new_row <- data.frame(
      Date = format(input$date, "%m-%d-%Y"),
      Category = input$category,
      Description = input$description,
      `Payment Method` = input$payment,
      Amount = input$amount,
      Notes = input$notes
    )
    
    sheet_append(sheet_id, new_row)
    
    # Reset inputs
    updateDateInput(session, "date", value = Sys.Date())
    updateSelectInput(session, "category", selected = "")
    updateTextInput(session, "description", value = "")
    updateSelectInput(session, "payment", selected = "")
    updateNumericInput(session, "amount", value = 0)
    updateTextAreaInput(session, "notes", value = "")
    
    trigger(trigger() + 1)
  })
}

shinyApp(ui = ui, server = server)
