library("shiny")
library("DT")
library("shinyauthr")
library("tibble")
library("RMySQL")
library("DBI")
library("pool")


AAAfile <- read.csv("Sample_Data.csv")
colnames(AAAfile) <- c("Company_Name","Sector","company_id_formatted","HQ_Location","CA100+","Research_Date","Year","GHG_Value","GHG_Unit","Activity_Value","Activity_Unit","GHG_Intensity")
row.names(AAAfile) <- AAAfile$`Company Name`
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

CountryList<-c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic (CAR)", "Chad", "Chile", "China", "Colombia", "Comoros", "Democratic Republic of the Congo", "Republic of the Congo", "Costa Rica", "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini (formerly Swaziland)", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar (formerly Burma)", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "North Macedonia (formerly Macedonia)", "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Korea", "South Sudan")


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
          
          
          ##########################################################################
          ###Tab 1###
          tabsetPanel( 
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
            
            tabPanel("Historical Calculation",
                     ###Company Details
                     
                     #tabsetpanels?
                     
                     
                     
                     h1("Own intensity calculation"),
                     submitButton("Submit", icon("refresh")),
                     textOutput("emissionsIntensity"),
                     #output telling you difference between calculated and reported intensity
                     
                     radioButtons("disclosureVSownestimate", "Is the disclosed intensity substantiated by own estimated intensity?", c("Yes"="yes", "No"="no"),selected ="no")
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
            tabPanel("Target Calculation",
                     actionButton("display", "Display Table"),
                     dataTableOutput('calcTable'),
                     
                     
                     
                     h1("Intensity target"),
                     
                     h1("Absolute target"),
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
  
  observeEvent(input$display, {
    if (!is.null(selected_company_id_2())) {
      
      query <- sprintf("SELECT c.company_name, e.timestamp_data_entry, e.ghg_scope, e.ghg_activity_boundary, e.year, e.ghg_value
                      FROM emissions_data e
                      INNER JOIN companies c ON e.company_id = c.id
                      WHERE e.year BETWEEN 2013 AND YEAR(CURDATE()) AND c.id = %d
                      ORDER BY c.company_name, e.year", selected_company_id_2())
      data <- dbGetQuery(pool, query)
      
      if (nrow(data) > 0) {
        output$calcTable <- renderDataTable({
          datatable(data, options = list(pageLength = 25))
        })
      } else {
        showNotification("No data found for the selected company in the specified year range", type = "error")
      }
    } else {
      showNotification("Error: No company selected or selected company not found in the database", type = "error")
    }
  })
}

  


shinyApp(ui = ui, server = server)