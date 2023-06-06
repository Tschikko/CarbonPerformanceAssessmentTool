library("shiny")
library("DT")
library("shinyauthr")
library("tibble")
library("RMySQL")
library("DBI")
library("pool")
library("rhandsontable")



########################
###Data Database
pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "cp_motherdatabas",
  host = "db4free.net",
  username = "tschikko",
  password = "bodsyz-Zynvig-judne5",
  port = 3306
)

CountryList<-c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic (CAR)", "Chad", "Chile", "China", "Colombia", "Comoros", "Democratic Republic of the Congo", "Republic of the Congo", "Costa Rica", "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini (formerly Swaziland)", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar (formerly Burma)", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "North Macedonia (formerly Macedonia)", "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", 
               "South Korea", "South Sudan","United Kingdom", "United States")


########################

###User Database
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  user_info = c("User One", "User Two")
)

ui <- fluidPage(
  
  ####################################
  ####Login
  shinyauthr::loginUI(id = "login_ui"),
  div(
    textInput("user_auth", label = NULL, value = FALSE),
    style = "display: none;"
  ),
  uiOutput("app_content")
)




server <- function(input, output, session){
  
  #################################################################  
  #################################################################
  #Company selection automatic update
  company_names <- reactiveVal()
  
  update_company_names <- function() {
    query <- "SELECT Company_Name FROM companies"
    result <- dbGetQuery(pool, query)
    company_names(c("Please select company name", result$Company_Name))
  }
  
  observe({
    update_company_names() 
  })
  
  observeEvent(input$addNewCompany, {
    update_company_names()
  })
  
  
  
  
  #################################################################
  #################################################################
  #App code
  output$app_content <- renderUI({
    if (credentials()$user_auth) {
      tagList(
        conditionalPanel(
          condition = "input.user_auth",
          titlePanel("Carbon Perfomance"),
          
          # Add your logo here
          div(
            tags$img(src = "OpenCo Logo.jpg", height = "50px", width = "75px"),
            style = "position: absolute; top: 10px; right: 10px;"
          ),

          
          tabsetPanel( 
          ##########################################################################
          ###Tab 0###
          tabPanel("Welcome", 
                   h1("Welcome to the Carbon Performance Assessment Platform!"),
                   h3("Our platform is designed to help users evaluate the carbon performance of companies across a variety of sectors. Through the integration of comprehensive carbon emissions data and sector-specific parameters, we provide a detailed, yet user-friendly approach to carbon performance analysis."),
                   
                   h3("Key Features:"),
                   HTML("
         <ul>
           <li><b>Company Selection:</b> Choose from a vast array of companies and review their carbon performance over different assessment periods.</li>
           <li><b>Historical Data:</b> Dive into the historical carbon emissions data for the selected company and explore detailed breakdowns of absolute and intensity data.</li>
           <li><b>Company Pathway:</b> Visualize the carbon performance pathway for any chosen company, including graphical representations and performance tables.</li>
           <li><b>Target Data:</b> Understand the future ambitions of companies through their carbon reduction targets.</li>
           <li><b>Target Calculation:</b> Analyze the intensity and absolute carbon reduction targets to better understand companies' future ambitions.</li>
         </ul>"),
                   
                   h3("How to Use this Platform"),
                   HTML("
         <ol>
           <li>Login with your credentials to gain access to the platform.</li>
           <li>Select a company from the dropdown menu under the 'Company Selection' tab.</li>
           <li>Review the historical data of the company and observe the company's pathway.</li>
           <li>Dive into the target data and calculations to better understand the company's carbon reduction commitments.</li>
           <li>Add new company data or update existing data to help us improve the platform.</li>
         </ol>"),
                   
                   h3("Remember, understanding a company's carbon performance is crucial to making informed decisions about how to better tackle climate change. Thank you for joining us in this important endeavor!")
          ),
          
          
          
          ##########################################################################
          ###Tab 1###

            tabPanel("Company Selection", 
                     h1("Update existing company"),
                     selectInput("companySelection", "Pick a company to perform a Carbon Performance assessment for:",
                                 choices = c("Please select company", company_names())),
                     submitButton("Select Company"),
                     
                     h1("Add new company"),
                     textOutput("newCompanyIntro"),
                     
                     textInput("newCompanyName", "Please enter the name of the company you would like to add"),
                     selectInput("newCompanyHQ", "Please enter the headquarter location of the company you would like to add",
                                 choices = c("Please select Country", CountryList)),
                     selectInput("newCompanyCA100", "Please add whether this company is a CA100+ focus company or not",
                                 choices = c("Yes" = 1,
                                             "No" = 0)),
                     selectInput("newCompanySector", "Please select the sector of the company you would like to add",
                                 choices = c("Please select sector", "Electricity Utilities")),
                     
                     actionButton("addNewCompany", "Add new company")
            ),
            
            
            ##########################################################################
            ###Tab 2###
            tabPanel("Company Pathway",
                     
                     
                     actionButton("plotButton", "Plot intensity pathway"),
                     
                     
                     plotOutput("intensityPathwayPlot"),
                     
                     
                     
                     selectInput("companyAssessmentDate", "Please select the assessment date for which you would like to see data. Please note, once you submit new data, a new assessment date will appear below",
                                 choices = c("Please select date", AAAfile$Research_Date)),
                     
                     submitButton("See company details"),
                     

                     
                     radioButtons("data_type", "Select data type:",
                                  choices = list("Absolute Emissions" = "emissions_data",
                                                 "Output Data" = "output_data",
                                                 "Intensity Data" = "intensity_data")),
                     dataTableOutput("company_data_table"),
                     
                     
                     
                     
                     tableOutput('CP_Table'),
                     plotOutput("carbonPerformance"),
                     plotOutput('CP_Pathway')
                     
            ),
            
            
            ##########################################################################
            ###Tab 3###
            tabPanel("Historical Data",
                     
                     ###>>>Emissions Data Input
                     h1("Absolute data"),
                     textOutput("emissionsDataCollection"),
                     textInput("emissionsYear", "Please provide the calendar year for which you are providing data"),                     
                     numericInput("ab_emissions","Absolute Emissions", value = NULL),
                     selectInput("ghg_measure", "Select reported GHG emissions type",
                                 c("CO2e",
                                   "CO2",
                                   "CH4",
                                   "N2O",
                                   "Other"="other")),
                     selectInput("ghg_unit", "Select the unit of the reported GHG emissions",
                                 c("tonnes",
                                   "kg",
                                   "US short tonnes",
                                   "N2O",
                                   "Other"="other")),
                     selectInput("ghg_scope", "Select reported GHG emissions scope",
                                 c("Scope 1 from own electricity Generation"="scope 1 from own electricity generation",
                                   "Scope 1" = "scope 1",
                                   "Scope 2" = "scope 2",
                                   "Scope 1 and 2" = "scope 1 and 2",
                                   "Unknown"="unkown")),
                     selectInput("emissionsAccountingBoundary", "Select accounting boundary on which emissions are stated",
                                 c("Equity" = "equity",
                                   "Operational" = "operational",
                                   "Unknown"="unkown")),
                     textInput("emissionsSource", "Please provide the name of the source document"),
                     numericInput("emissionsSourcePage","Please provide the source page",value = NULL),
                     dateInput("emissionsSourceDate", "Source publication date:", value = "2022-06-01", format = "mm/dd/yy"),
                     textInput("emissionsURL", "Please provide the URL of the source document or website"),
                     textAreaInput("emissionsComments", "Please provide any further comments including sources where appropriate"),
                     actionButton("submitHistoricalData", "Submit"),
                     
                     
                     
                     ###>>>Company output Data Input 
                     textOutput("outputDataCollection"),
                     textInput("outputYear", "Please provide the calendar year for which you are providing data"),   
                     numericInput("output","Output", value = NULL),
                     selectInput("output_unit", "Select the unit of the reported GHG emissions",
                                 c("GWh",
                                   "MWh",
                                   "kWh",
                                   "GJ",
                                   "MJ",
                                   "MBtu",
                                   "TOE",
                                   "BOE",
                                   "Other"="other")),
                     selectInput("outputAccountingBoundary", "Select accounting boundary on which the output is stated",
                                 c("Equity" = "equity",
                                   "Operational" = "operational",
                                   "Unknown"="unkown")),
                     textInput("outputSource", "Please provide the name of the source document"),
                     numericInput("outputSourcePage","Please provide the source page",value = NULL),
                     dateInput("outputSourceDate", "Source publication date:", value = "2022-06-01", format = "mm/dd/yy"),
                     textInput("outputURL", "Please provide the URL of the source document or website"),
                     textAreaInput("outputComments", "Please provide any further comments including sources where appropriate"),
                     actionButton("submitOutputData", "Submit"),
                     
                     
                     
                     
                     
                     fileInput("additionalIntensityCalculations", "Please upload any files including additional permutations or modifications made to disclosed values. Please follow the format you can find here."),
                     
                     
                     
                     
                     h1("Intensity data"),
                     ###>>>Company intensity Data Input 
                     textOutput("intensityDataCollection"),
                     textInput("intensityYear", "Please provide the calendar year for which you are providing data"),   
                     numericInput("intensity","Intensity", value = NULL),
                     textInput("intensityNumeratorUnit", "Please provide the numerator unit of the intensity value you are providing"),   
                     textInput("intensityDenominatorUnit", "Please provide the denominator unit of the intensity value you are providing"),                       
                     selectInput("intensityAccountingBoundary", "Select accounting boundary on which the emissions intensity is stated",
                                 c("Equity" = "equity",
                                   "Operational" = "operational",
                                   "Unknown"="unkown")),
                     textInput("intensitySource", "Please provide the name of the source document"),
                     numericInput("intensitySourcePage","Please provide the source page",value = NULL),
                     dateInput("intensitySourceDate", "Source publication date:", value = "2022-06-01", format = "mm/dd/yy"),
                     textInput("intensityURL", "Please provide the URL of the source document or website"),
                     textAreaInput("intensityComments", "Please provide any further comments including sources where appropriate"),
                     actionButton("submitIntensityData", "Submit"),
            ),
            
            
            
            ##########################################################################
            ###Tab 4### 
            tabPanel("Target Data",
                     textInput("target_name", "Target Name"),
                     numericInput("target_year", "Target Year", value = 2000, min = 1900, max = 2100),
                     numericInput("target_value", "Target Value", value = 0),
                     textInput("target_unit", "Target Unit"),
                     textInput("target_scope", "Target Scope"),
                     numericInput("base_year", "Base Year", value = 2000, min = 1900, max = 2100),
                     textInput("target_status", "Target Status"),
                     dateInput("targets_disclosure_date", "Disclosure Date"),
                     textInput("targets_source_doc", "Source Document"),
                     numericInput("targets_source_page", "Source Page", value = 1, min = 1),
                     dateInput("targets_source_date", "Source Date"),
                     textInput("targets_comment", "Comment"),
                     numericInput("target_reduction_percent", "Targeted Reduction Percentage", value = NULL),
                     numericInput("target_reduction_absolute", "Targeted Reduction Absolute", value = NULL),
                     textInput("target_url", "Target URL"),
                     actionButton("add_target", "Add GHG Target"),
                     
            ),
            
            
            ##########################################################################
            ###Tab 5### 
            tabPanel("Calculation",
                     h1("Historical Intensities"),
                     
                     br(),
                     
                     actionButton("display", "Display Table"),
                     rHandsontableOutput('calcTable'),
                     

                     
                     actionButton("calcButton", "Calculate Emissions Intensity"),  # This is the new button
                     
                     
                     DT::dataTableOutput('selectedTable'),
                     
                     # Create a dropdown menu in the UI
                     selectInput("statusDropDown", "Status", 
                                 choices = c("1st draft assessment", 
                                             "internally reviewed assessment", 
                                             "company reviewed assessment", 
                                             "final assessment")),
                     
                     # Create a "Save to Database" button
                     actionButton("saveButton", "Save to Database"),
                     
                     
                     
                     h1("Targeted Intensities"),
                     
                     actionButton("display_target_Button", "Display Table"),
                     rHandsontableOutput('targetsTable'),
                     
                     
                     actionButton("calc_target_Button", "Calculate Target Intensity"),  # This is the new button
                     
     
                     DT::dataTableOutput('selected_target_Table'),
                     
                     selectInput("status_target_dropdown", "Assessment status:", 
                                 c("1st draft assessment", 
                                   "internally reviewed assessment", 
                                   "company reviewed assessment", 
                                   "final assessment")),
                     
                     actionButton("save_target_to_db", "Save to Database"),
                     
            )
          )
        )
      )
    }
  })
  
  
  
  #################################################################
  #################################################################
  #Text Sections
  
  output$newCompanyIntro <- renderText({
    paste("If the company you are researching is not yet covered, please create an entry below:")
  })
  output$companyDetails <- renderText({
    paste("Company Name: SSE >", "Sector: Electricity Sector>", "ID: 1>", "Headquarter Location: UK>", "Company Size: Large>, CA100+ Focus Company: Yes")
  })
  output$emissionsDataCollection <- renderText({
    paste("When collecting emissions data please ensure the data only reflects scope 1 emissions from the company's owned electricity generation, exlcuding any emissions from non-electricity generation related activities (e.g. such as from a gas distribution business run by the same company) or emissions relating to electricity purchased by the company for the sole purpse of re-selling it. For more information on emission scopes please click here. Key source documents usually are: Annual reports (or 10-K's/20F's in the US), Sustainability reports or similar, Climate specific reports (e.g. climate roadmap), Transition reports, investor presentations, websites and CDP responses. In the US please consult IEEE ESG templates as a key source if provided by the company.")
  })
  output$outputDataCollection <- renderText({
    paste("When collecting output data please ensure the data only reflects the company's owned electricity generation stated on the same accounting boundary as the emissions data, exlcuding electricity purchased by the company for the sole purpse of re-selling it.")
  })
  output$emissionsIntensity <- reactive({
    input$emissions/input$output
  })
  

  
  #################################################################
  #################################################################
  #Database code
  
  credentials <- shinyauthr::loginServer(
    id = "login_ui",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    sodium_hashed = FALSE
  )

  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
      query <- "SELECT Company_Name FROM companies"
      result <- dbGetQuery(pool, query)
      updateSelectInput(session, "companySelection", choices = c("Please select company name", result$Company_Name))
    } else {
      showNotification("Please log in to access the app.")
    }
  })
  
  #################################################################
  #Database input
  
  insert_data <- function(company_name, sector, hq_location, ca100) {
    query <- sprintf("INSERT INTO companies (Company_Name, Sector, HQ_Location, CA100) 
                   VALUES ('%s', '%s', '%s', %s);",
                     company_name, sector, hq_location, ca100)
    
    cat("Query: ", query, "\n") # Print the SQL query
    
    result <- NULL
    tryCatch({
      poolWithTransaction(pool, {
        result <- dbExecute(pool, query)
      })
    }, error = function(e) {
      cat("ERROR: ", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
    
    return(result)
  }
  
  
  
  
  get_company_id <- function(pool, company_name) {
    query <- sprintf("SELECT id FROM companies WHERE Company_Name = '%s'", company_name)
    result <- dbGetQuery(pool, query)
    
    if (nrow(result) > 0) {
      return(result$id)
    } else {
      return(NULL)
    }
  }
  
  
  
  
  
  observeEvent(input$addNewCompany, {
    result <- insert_data(
      company_name = input$newCompanyName,
      sector = input$newCompanySector,
      hq_location = input$newCompanyHQ,
      ca100 = input$newCompanyCA100
    )
    if (is.null(result)) {
      showNotification("Error adding new company. Please check your input.", type = "error")
    } else {
      showNotification("New company added successfully.")
      updateSelectInput(session, "companySelection", choices = c("Please select company name", dbGetQuery(pool, "SELECT Company_Name FROM companies")))
    }
  })
  
  
  
  
  observeEvent(reactive(credentials()$user_auth), {
    updateTextInput(session, "user_auth", value = credentials()$user_auth)
    if (credentials()$user_auth) {
      query <- "SELECT Company_Name FROM companies"
      result <- dbGetQuery(pool, query)
      updateSelectInput(session, "companySelection", choices = c("Please select company name", result$Company_Name))
    }
  }, ignoreNULL = FALSE)
  
  
  
  
  
  
  
  
  
  observeEvent(input$submitHistoricalData, {
    # Get the selected company_name and fetch the corresponding company_id
    selected_company_name <- input$companySelection
    selected_company_id <- get_company_id(pool, selected_company_name)
    
    # Make sure a company is selected
    if (!is.null(selected_company_id)) {
      # Prepare the data for insertion
      new_data <- list(
        company_id = selected_company_id,
        ghg_scope = if (is.null(input$ghg_scope)) "" else input$ghg_scope,
        ghg_activity_boundary = if (is.null(input$emissionsAccountingBoundary)) "" else input$emissionsAccountingBoundary,
        ghg_source_doc = if (is.null(input$emissionsSource)) "" else input$emissionsSource,
        ghg_source_page = if (is.null(input$emissionsSourcePage)) 0 else input$emissionsSourcePage,
        ghg_source_date = if (is.null(input$emissionsSourceDate)) "" else as.character(input$emissionsSourceDate),
        ghg_comment = if (is.null(input$emissionsComments)) "" else input$emissionsComments,
        ghg_value = if (is.null(input$ab_emissions)) 0 else input$ab_emissions,
        year = if (is.null(input$emissionsYear)) 0 else input$emissionsYear,
        ghg_type = if (is.null(input$ghg_type)) "" else input$ghg_type,
        ghg_url = if (is.null(input$emissionsURL)) "" else input$emissionsURL,
        ghg_measure = if (is.null(input$ghg_measure)) "" else input$ghg_measure,
        ghg_unit = if (is.null(input$ghg_unit)) "" else input$ghg_unit
      )
      
      
      # Insert the data into the 'emissions_data' table
      query <- sprintf("INSERT INTO emissions_data (company_id, ghg_scope, ghg_activity_boundary, ghg_source_doc, ghg_source_page, ghg_source_date, ghg_comment, ghg_value, ghg_measure, ghg_unit, ghg_type, ghg_url, year) VALUES (%d, '%s', '%s', '%s', %d, '%s', '%s', %f, '%s', '%s', '%s', '%s','%s')",
                       new_data$company_id,
                       new_data$ghg_scope,
                       new_data$ghg_activity_boundary,
                       new_data$ghg_source_doc,
                       new_data$ghg_source_page,
                       new_data$ghg_source_date,
                       new_data$ghg_comment,
                       new_data$ghg_value,
                       new_data$ghg_measure,
                       new_data$ghg_unit,
                       new_data$ghg_type,
                       new_data$ghg_url,
                       new_data$year)
      
      
      
      tryCatch({
        dbExecute(pool, query)
        
        # Show success message
        showNotification("Data successfully submitted.", type = "message", duration = 3)
        
        # Clear the input fields (optional)
        updateNumericInput(session, "ab_emissions", value = NULL)
        updateTextInput(session, "emissionsSource", value = "")
        updateNumericInput(session, "emissionsSourcePage", value = NULL)
        updateTextAreaInput(session, "emissionsComments", value = "")
        updateTextInput(session, "emissionsURL", value = NULL)
        # Clear the rest of the input fields
      }, error = function(e) {
        # Show error message with the actual error
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    } else {
      showNotification("Please select a company first.", type = "error", duration = 5)
    }
  })
  
  
  # Function to insert output data into the 'output_data' table
  insert_output_data <- function(pool, company_id, data_type, year, output_measure, output_activity_boundary, output_value, output_unit, output_source_doc, output_source_page, output_source_date, output_comment, disclosure_date, output_url) {
    query <- sprintf("INSERT INTO output_data (company_id, data_type, year, output_measure, output_activity_boundary, output_value, output_unit, output_source_doc, output_source_page, output_source_date, output_comment, disclosure_date, output_url) VALUES (%d, '%s', %d, '%s', '%s', %f, '%s', '%s', %d, '%s', '%s', '%s', '%s')",
                     company_id, data_type, year, output_measure, output_activity_boundary, output_value, output_unit, output_source_doc, output_source_page, output_source_date, output_comment, disclosure_date, output_url)
    
    result <- NULL
    tryCatch({
      poolWithTransaction(pool, {
        result <- dbExecute(pool, query)
      })
    }, error = function(e) {
      cat("ERROR: ", conditionMessage(e), "\n")
      showNotification(paste("Error:", conditionMessage(e)), type = "error")
    })
    
    return(result)
  }
  
  
  
  
  
  
  
  
  observeEvent(input$submitOutputData, {
    # Get the selected company_name and fetch the corresponding company_id
    selected_company_name <- input$companySelection
    selected_company_id <- get_company_id(pool, selected_company_name)
    
    # Make sure a company is selected and the company_id is an integer
    if (!is.null(selected_company_id) && is.integer(selected_company_id)) {
      query <- sprintf("INSERT INTO output_data (company_id, year, output_value, output_unit, output_activity_boundary, output_source_doc, output_source_page, output_source_date, output_comment) VALUES (%d, %d, %f, '%s', '%s', '%s', %d, '%s', '%s')",
                       selected_company_id,
                       as.integer(input$outputYear),
                       input$output,
                       input$output_unit,
                       input$outputAccountingBoundary,
                       input$outputSource,
                       as.integer(input$outputSourcePage),
                       as.character(input$outputSourceDate),
                       input$outputComments)
      
      tryCatch({
        dbExecute(pool, query)
        
        # Show success message
        showNotification("Output data successfully submitted.", type = "message", duration = 3)
        
        # Clear the input fields (optional)
        updateTextInput(session, "outputYear", value = NULL)
        updateNumericInput(session, "output", value = NULL)
        updateTextInput(session, "outputSource", value = "")
        updateNumericInput(session, "outputSourcePage", value = NULL)
        updateTextAreaInput(session, "outputComments", value = "")
        # Clear the rest of the input fields
      }, error = function(e) {
        # Show error message with the actual error
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    } else {
      showNotification("Please select a company first or make sure the company ID is valid.", type = "error", duration = 5)
    }
  })
  
  
  
  
  
  
  
  observeEvent(input$submitIntensityData, {
    # Get the selected company_name and fetch the corresponding company_id
    selected_company_name <- input$companySelection
    selected_company_id <- get_company_id(pool, selected_company_name)
    
    # Make sure a company is selected and the company_id is an integer
    if (!is.null(selected_company_id) && is.integer(selected_company_id)) {
      query <- sprintf("INSERT INTO intensity_data (company_id, year, intensity_value, intensity_numerator_unit, intensity_denominator_unit, intensity_scope, intensity_source_doc, intensity_source_page, intensity_source_date, intensity_comment, intensity_source_url) VALUES (%d, %d, %f, '%s', '%s', '%s', '%s', %d, '%s', '%s', '%s')",
                       selected_company_id,
                       as.integer(input$intensityYear),
                       input$intensity,
                       input$intensityNumeratorUnit,
                       input$intensityDenominatorUnit,
                       input$intensityAccountingBoundary,
                       input$intensitySource,
                       as.integer(input$intensitySourcePage),
                       as.character(input$intensitySourceDate),
                       input$intensityComments,
                       input$intensityURL)
      
      tryCatch({
        dbExecute(pool, query)
        
        # Show success message
        showNotification("Intensity data successfully submitted.", type = "message", duration = 3)
        
        # Clear the input fields (optional)
        updateTextInput(session, "intensityYear", value = NULL)
        updateNumericInput(session, "intensity", value = NULL)
        updateTextInput(session, "intensityNumeratorUnit", value = "")
        updateTextInput(session, "intensityDenominatorUnit", value = "")
        updateTextInput(session, "intensitySource", value = "")
        updateNumericInput(session, "intensitySourcePage", value = NULL)
        updateTextAreaInput(session, "intensityComments", value = "")
        # Clear the rest of the input fields
      }, error = function(e) {
        # Show error message with the actual error
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    } else {
      showNotification("Please select a company first or make sure the company ID is valid.", type = "error", duration = 5)
    }
  })
  
  
  
  get_company_id <- function(pool, company_name) {
    query <- sprintf("SELECT id FROM companies WHERE Company_Name = '%s'", company_name)
    result <- dbGetQuery(pool, query)
    
    if (nrow(result) > 0) {
      return(result$id)
    } else {
      return(NULL)
    }
  }
  
  
  
  
  
  
  observeEvent(input$add_target, {
    # Get the selected company_name and fetch the corresponding company_id
    selected_company_name <- input$companySelection
    selected_company_id <- get_company_id(pool, selected_company_name)
    
    # Make sure a company is selected and the company_id is an integer
    if (!is.null(selected_company_id) && is.integer(selected_company_id)) {
      query <- sprintf("INSERT INTO ghg_reduction_targets (company_id, target_name, target_year, target_value, target_unit, target_scope, base_year, target_status, disclosure_date, source_doc, source_page, source_date, target_comment, targeted_reduction_percent, targeted_reduction_absolute, target_url) VALUES (%d, '%s', %d, %f, '%s', '%s', %d, '%s', '%s', '%s', %d, '%s', '%s', %f, %f, '%s')",
                       selected_company_id,
                       input$target_name,
                       as.integer(input$target_year),
                       input$target_value,
                       input$target_unit,
                       input$target_scope,
                       as.integer(input$base_year),
                       input$target_status,
                       as.character(input$targets_disclosure_date),
                       input$targets_source_doc,
                       as.integer(input$targets_source_page),
                       as.character(input$targets_source_date),
                       input$targets_comment,
                       input$target_reduction_percent,
                       input$target_reduction_absolute,
                       input$target_url
      )
      
      tryCatch({
        dbExecute(pool, query)
        
        # Show success message
        showNotification("Target data successfully submitted.", type = "message", duration = 3)
        
        # Clear the input fields (optional)
        # You can add code to clear input fields here if you'd like
        
      }, error = function(e) {
        # Show error message with the actual error
        showNotification(paste("Error:", e$message), type = "error", duration = 5)
      })
    } else {
      showNotification("Please select a company first or make sure the company ID is valid.", type = "error", duration = 5)
    }
  })
  
  
  
  
  
  
  
  
  

#################################################################
#Database output


  selected_company_id <- reactive({
    selected_name <- input$companySelection
    if (selected_name == "Please select company") {
      return(NULL)
    }
    get_company_id(pool, selected_name)
  })
  
  output$company_data_table <- renderDataTable({
    req(selected_company_id())
    req(input$data_type)
    
    data_type <- input$data_type
    query <- sprintf("SELECT * FROM %s WHERE company_id = %d", data_type, selected_company_id())
    
    data <- dbGetQuery(pool, query)
    datatable(data)
  })
  
  
  
  # Get company id from company name
  get_company_id <- function(pool, company_name) {
    query <- sprintf("SELECT id FROM companies WHERE company_name = '%s'", company_name)
    return(dbGetQuery(pool, query)$id)
  }
  
  # Define a reactive expression for selected_company_id
  selected_company_id_2 <- reactive({
    selected_name <- input$companySelection
    if (selected_name == "Please select company") {
      return(NULL)
    }
    company_id <- get_company_id(pool, selected_name)
    if (!is.null(company_id)) {
      company_id[[1]]
    } else {
      return(NULL)
    }
  })
  
  
  
  
  
  data <- reactiveValues(df = NULL)
  
  observeEvent(input$display, {
    if (!is.null(selected_company_id_2())) {
      query <- sprintf("SELECT e.company_id, c.company_name, e.timestamp_data_entry, e.ghg_scope, e.ghg_activity_boundary, e.year, e.ghg_value,
                        'Absolute Emissions' AS data_type, NULL AS output_measure, NULL AS output_activity_boundary, NULL AS output_value, NULL AS output_unit, NULL AS output_source_doc, NULL AS output_source_page, NULL AS output_source_date, NULL AS output_comment
                        FROM emissions_data e
                        INNER JOIN companies c ON e.company_id = c.id
                        WHERE e.year BETWEEN 2013 AND YEAR(CURDATE()) AND e.company_id = %d
                        UNION
                        SELECT o.company_id, c.company_name, o.timestamp_data_entry, NULL AS ghg_scope, NULL AS ghg_activity_boundary, o.year, NULL AS ghg_value,
                        'Output' AS data_type, o.output_measure, o.output_activity_boundary, o.output_value, o.output_unit, o.output_source_doc, o.output_source_page, o.output_source_date, o.output_comment
                        FROM output_data o
                        INNER JOIN companies c ON o.company_id = c.id
                        WHERE o.year BETWEEN 2013 AND YEAR(CURDATE()) AND o.company_id = %d
                        ORDER BY company_name, year", selected_company_id_2(), selected_company_id_2())
      
      data$df  <- dbGetQuery(pool, query)
      
      if (nrow(data$df) > 0) {
        # Add radio button column
        data$df$Selected <- FALSE
        
        output$calcTable <- renderRHandsontable({
          rhandsontable(data$df, rowHeaders = FALSE, width = "100%", height = 400) %>%
            hot_col("Selected", type = "checkbox", readOnly = FALSE)
        })
      } else {
        showNotification("No data found for the selected company in the specified year range", type = "error")
      }
    } else {
      showNotification("Error: No company selected or selected company not found in the database", type = "error")
    }
  })
 
  
  # Initialize reactiveValues object
  results_df <- reactiveValues(df = NULL)
  
  observeEvent(input$calcButton, {
    # Get the data from the rHandsontable
    calcTableData <- hot_to_r(input$calcTable)
    
    # Convert the data to a dataframe
    df <- as.data.frame(calcTableData)
    
    # Extract the rows where Selected is TRUE
    selected_rows <- df[df$Selected == TRUE, ]
    
    # Get a list of unique years from the selected rows
    unique_years <- unique(selected_rows$year)
    
    # Initialize an empty data frame for the results
    results_df$df <- data.frame()
    
    for (year in unique_years) {
      # For each year, get the selected "output" and "absolute emissions" rows
      yearly_rows <- selected_rows[selected_rows$year == year, ]
      output_row <- yearly_rows[yearly_rows$data_type == "Output", ]
      emission_row <- yearly_rows[yearly_rows$data_type == "Absolute Emissions", ]
      
      # If exactly one of each row is selected, calculate the intensity and add a row to the results dataframe
      if(nrow(output_row) == 1 && nrow(emission_row) == 1) {
        intensity <- as.numeric(emission_row$ghg_value) / as.numeric(output_row$output_value)
        results_df$df <- rbind(results_df$df, data.frame(
          company_id = as.numeric(emission_row$company_id),
          company_name = as.character(emission_row$company_name),
          year = as.numeric(year),
          absolute_emissions = as.numeric(emission_row$ghg_value),
          output = as.numeric(output_row$output_value),
          intensity = intensity
        ))
      }
    }
    
    # Render the results as a DataTable
    output$selectedTable <- DT::renderDataTable({
      DT::datatable(results_df$df)
    })
  })
  
  observeEvent(input$saveButton, {
    
    # Drop rows with any NULL values
    results_df$df <- results_df$df[complete.cases(results_df$df), ]
    
    
    if (nrow(results_df$df) == 0) {
      showNotification("No data to save. Please select data first.", type = "error")
      return()
    }
    
    if (is.null(input$statusDropDown) || input$statusDropDown == "") {
      showNotification("No status selected. Please select a status first.", type = "error")
      return()
    }
    
    
    
    # Assign status value based on the selected value in the dropdown
    results_df$df$status <- input$statusDropDown
    
    if(anyNA(results_df$df)) {
      print("There are NA values in the dataframe")
    }
    
    # Check the database connection
    if (!dbIsValid(pool)) {
      print("The database connection is not valid.")
      return()
    }
    
    # Try a simpler query
    tryCatch({
      simple_query <- "SELECT * FROM company_pathway_hist LIMIT 5"
      simple_results <- dbGetQuery(pool, simple_query)
      print(simple_results)
    }, error = function(e) {
      print("Error running simple query:")
      print(e)
    })
    
    # Loop over the rows of the dataframe
    for(i in 1:nrow(results_df$df)) {
      row <- results_df$df[i, ]
      
      # Skip this row if any of the columns contain NULL values
      if(any(is.null(row))) {
        print(paste("Skipping row", i, "due to NULL values"))
        print(row)
        next
      }
      
      # Prepare the INSERT statement
      query <- sprintf("INSERT INTO company_pathway_hist (company_id, company_name, year, absolute_emissions, output, intensity, status) VALUES (%d, '%s', %d, %f, %f, %f, '%s')",
                       row$company_id,
                       row$company_name,
                       row$year,
                       row$absolute_emissions,
                       row$output,
                       row$intensity,
                       row$status
      )
      
      # Print the query
      print(query)
      
      # Execute the INSERT statement and store the result
      result <- tryCatch({
        dbExecute(pool, query)
      }, error = function(e) {
        # If there's an error, print it and show a notification
        print(paste("Error running INSERT query:", e$message))
        showNotification("Error while saving to the database. Please check your inputs.", type = "error")
      })
      
      # If the result is not an error, show a success notification
      if (!inherits(result, "error")) {
        showNotification("Data saved successfully to the database.", type = "message")
      }
    }
  })
  
  # Create a reactive value to hold the database table data
  ghg_targets <- reactiveValues(df = NULL)
  
  # Fetch the data when a button is clicked or some other event occurs
  observeEvent(input$display_target_Button, {
    if (!is.null(selected_company_id())) {
      query <- sprintf("SELECT * FROM ghg_reduction_targets WHERE company_id = %d", selected_company_id())
      
      ghg_targets$df <- dbGetQuery(pool, query)
      
      if (nrow(ghg_targets$df) > 0) {
        # Add checkbox column
        ghg_targets$df$Selected <- FALSE
        
        output$targetsTable <- renderRHandsontable({
          rhandsontable(ghg_targets$df, rowHeaders = FALSE, width = "100%", height = 400) %>%
            hot_col("Selected", type = "checkbox", readOnly = FALSE)
        })
      } else {
        showNotification("No data found for the selected company", type = "error")
      }
    } else {
      showNotification("Error: No company selected or selected company not found in the database", type = "error")
    }
  })
  
  
  
  # Initialize reactiveValues object for target data
  target_results_df <- reactiveValues(df = NULL)
  
  observeEvent(input$calc_target_Button, {
    # Get the data from the rHandsontable
    calcTableData <- hot_to_r(input$targetsTable)
    
    # Convert the data to a dataframe
    df <- as.data.frame(calcTableData)
    
    # Extract the rows where Selected is TRUE
    selected_rows <- df[df$Selected == TRUE, ]
    
    for (i in 1:nrow(selected_rows)) {
      # Get the base year intensity from the company_pathway_hist table
      base_year <- selected_rows$base_year[i]
      base_intensity_query <- sprintf("SELECT intensity FROM company_pathway_hist WHERE company_id = %d AND year = %d", selected_rows$company_id[i], base_year)
      base_intensity <- as.numeric(dbGetQuery(pool, base_intensity_query)$intensity)
      
      print(paste("Base intensity:", base_intensity))
      
      # Calculate the targeted intensity
      targeted_intensity <- base_intensity * (1 + selected_rows$targeted_reduction_percent[i])
      
      print(paste("Targeted intensity:", targeted_intensity))
      
      # Add a row to the results dataframe
      target_results_df$df <- rbind(target_results_df$df, data.frame(
        company_id = selected_rows$company_id[i],
        company_name = selected_rows$target_name[i],
        base_year = base_year,
        target_year = selected_rows$target_year[i],
        targeted_reduction_percent = selected_rows$targeted_reduction_percent[i],
        targeted_intensity = targeted_intensity
      ))
    }
    
    # Render the results as a DataTable
    output$selected_target_Table <- DT::renderDataTable({
      DT::datatable(target_results_df$df)
    })
  })
  
  # In your server function
  observeEvent(input$save_target_to_db, {
    status_target <- input$status_target_dropdown
    
    if (nrow(target_results_df$df) == 0) {
      showNotification("No data to save. Please select data first.", type = "error")
      return()
    }
    
    if (is.null(status_target) || status_target == "") {
      showNotification("No status selected. Please select a status first.", type = "error")
      return()
    }
    
    # Assign status value based on the selected value in the dropdown
    target_results_df$df$status_target <- status_target
    
    if(anyNA(target_results_df$df)) {
      print("There are NA values in the dataframe")
    }
    
    # Check the database connection
    if (!dbIsValid(pool)) {
      print("The database connection is not valid.")
      return()
    }
    
    # Loop over the rows of the dataframe
    for(i in 1:nrow(target_results_df$df)) {
      row <- target_results_df$df[i, ]
      
      # Skip this row if any of the columns contain NULL values
      if(any(is.null(row))) {
        print(paste("Skipping row", i, "due to NULL values"))
        print(row)
        next
      }
      
      # Prepare the INSERT statement
      query <- sprintf("INSERT INTO company_pathway_target (company_id, company_name, base_year, target_year, targeted_reduction_percent, targeted_intensity, status_target) VALUES (%d, '%s', %d, %d, %f, %f, '%s')",
                       row$company_id,
                       row$company_name,
                       row$base_year,
                       row$target_year,
                       row$targeted_reduction_percent,
                       row$targeted_intensity,
                       row$status_target
      )
      
      # Print the query
      print(query)
      
      # Execute the INSERT statement and store the result
      result <- tryCatch({
        dbExecute(pool, query)
      }, error = function(e) {
        # If there's an error, print it and show a notification
        print(paste("Error running INSERT query:", e$message))
        showNotification("Error while saving to the database. Please check your inputs.", type = "error")
      })
      
      # If the result is not an error, show a success notification
      if (!inherits(result, "error")) {
        showNotification("Data saved successfully to the database.", type = "message")
      }
    }
  })
  
  

  
  # Define a reactive expression for selected_company_id
  selected_company_id_3 <- reactive({
    req(input$companySelection)
    selected_name <- input$companySelection
    if (selected_name == "Please select company") {
      return(NULL)
    }
    
    # Use tryCatch when calling get_company_id
    company_id <- tryCatch({
      get_company_id(pool, selected_name)
    }, error = function(e) {
      print(paste("Error in get_company_id:", e$message))
      return(NULL)
    })
    
    if (!is.null(company_id)) {
      company_id[[1]]
    } else {
      return(NULL)
    }
  })
  
  
  output$intensityPathwayPlot <- renderPlot({
    # Check if the plotButton has been clicked. If not, exit this function.
    input$plotButton
    
    # Check if a company is selected
    if (is.null(selected_company_id_3()) || length(selected_company_id_3()) == 0 || selected_company_id_3() == "Please select company") {
      showNotification("Error: No company selected or selected company not found in the database", type = "error")
      return()
    }
    
    # Fetch the historical data from company_pathway_hist
    historical_query <- sprintf("SELECT year, intensity FROM company_pathway_hist WHERE company_id = %d", selected_company_id_3())
    historical_data <- dbGetQuery(pool, historical_query)
    
    # Fetch the target data from company_pathway_target
    target_query <- sprintf("SELECT target_year, targeted_intensity FROM company_pathway_target WHERE company_id = %d", selected_company_id_3())
    target_data <- dbGetQuery(pool, target_query)
    
    # Check if there's data to plot
    if (nrow(historical_data) == 0 && nrow(target_data) == 0) {
      showNotification("No data found for the selected company", type = "error")
      return()
    }
    
    # Plot the data
    plot(historical_data$year, historical_data$intensity, type = "l", 
         ylim = c(0, max(historical_data$intensity, target_data$targeted_intensity)), 
         xlim = c(2010, 2050),
         xlab = "Year", ylab = "Intensity", main = "Emissions Intensity Pathway")
    
    # Add a line for the targeted intensities
    lines(target_data$target_year, target_data$targeted_intensity, col = "red")
    
    # Connect the last point of historical data to the first point of target data
    if (nrow(historical_data) > 0 && nrow(target_data) > 0) {
      lines(x = c(tail(historical_data$year, n = 1), target_data$target_year[1]),
            y = c(tail(historical_data$intensity, n = 1), target_data$targeted_intensity[1]),
            col = "red")
    }
  })
  
  
  
  
  
  
  }
  
  shinyApp(ui, server)
  
  
  

