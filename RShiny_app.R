library("shiny")
library("DT")
library("shinyauthr")
library("tibble")
library("RMySQL")
library("DBI")
library("pool")


AAAfile <- read.csv("Sample_Data.csv")
colnames(AAAfile) <- c("Company_Name","Sector","Company_ID","HQ_Location","CA100+","Research_Date","Year","GHG_Value","GHG_Unit","Activity_Value","Activity_Unit","GHG_Intensity")
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
                                 choices = c("Please select company name", AAAfile$Company_Name)),
                     submitButton("Select Company"),
                     
                     h1("Add new company"),
                     textOutput("newCompanyIntro"),
                     
                     textInput("newCompanyName", "Please enter the name of the company you would like to add"),
                     selectInput("newCompanyHQ", "Please enter the headquarter location of the company you would like to add",
                                 choices = c("Please select Country", CountryList)),
                     selectInput("newCompanyCA100", "Please add whether this company is a CA100+ focus company or not",
                                 choices = c("Yes" = "yes",
                                             "No" = "no")),
                     selectInput("newCompanySector", "Please select the sector of the company you would like to add",
                                 choices = c("Please select sector", "Electricity Utilties")),
                     
                     actionButton("addNewCompany", "Add new company")
            ),
            
            
            ##########################################################################
            ###Tab 2###
            tabPanel("Company Pathway",
                     
                     selectInput("companyAssessmentDate", "Please select the assessment date for which you would like to see data. Please note, once you submit new data, a new assessment date will appear below",
                                 choices = c("Please select date", AAAfile$Research_Date)),
                     
                     submitButton("See company details"),
                     
                     plotOutput('CP_Pathway'),
                     tableOutput('CP_Table'),
                     plotOutput("carbonPerformance")
                     
            ),
            
            
            ##########################################################################
            ###Tab 3###
            tabPanel("Historical Data",
                     
                     ###>>>Emissions Data Input
                     h1("Absolute data"),
                     textOutput("emissionsDataCollection"),
                     numericInput("ab_emissions","Ab_Emissions", value = NULL),
                     selectInput("ghg_scope", "Select reported GHG emissions scope",
                                 c("Scope 1 from own electricity Generation"="scope 1 from own electricity generation",
                                   "Scope 1" = "scope 1",
                                   "Scope 2" = "scope 2",
                                   "Scope 1 and 2" = "scope 1 and 2",
                                   "Unknown"="unkown")),
                     selectInput("emissionsAccountingBoundary2020", "Select accounting boundary on which emissions are stated",
                                 c("Equity" = "equity",
                                   "Operational" = "operational",
                                   "Unknown"="unkown")),
                     textInput("emissionsSource2020", "Please provide the name of the source document"),
                     numericInput("emissionsSourcePage2020","Please provide the source page",value = NULL),
                     dateInput("emissionsSourceDate", "Source publication date:", value = "2022-06-01", format = "mm/dd/yy"),
                     ### Source URL
                     textAreaInput("emissionsComments2020", "Please provide any further comments including sources where appropriate"),
                     actionButton("submitHistoricalData", "Submit"),
                     
                     ###>>>Company output Data Input 
                     textOutput("outputDataCollection"),
                     numericInput("output2020","Output 2020", value = NULL),
                     selectInput("outputAccountingBoundary2020", "Select accounting boundary on which the output is stated",
                                 c("Equity" = "equity",
                                   "Operational" = "operational")),
                     textInput("outputSource2020", "Please provide the name of the source document"),
                     numericInput("outputSourcePage2020","Please provide the source page",value = NULL),
                     textInput("outputComments2020", "Please provide any further comments including sources where appropriate"),
                     
                     fileInput("additionalIntensityCalculations", "Please upload any files including additional permutations or modifications made to disclosed values. Please follow the format you can find here."),
                     
                     
                     h1("Intensity data"),
                     ###>>>Company intensity Data Input 
                     textOutput("intensityDataCollection"),
                     numericInput("intensity2020","Intensity 2020", value = NULL),
                     selectInput("intensityAccountingBoundary2020", "Select accounting boundary on which the emissions intensity is stated",
                                 c("Equity" = "equity",
                                   "Operational" = "operational")),
                     textInput("intensitySource2020", "Please provide the name of the source document"),
                     numericInput("intensitySourcePage2020","Please provide the source page",value = NULL),
                     textInput("intensityComments2020", "Please provide any further comments including sources where appropriate")
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
                     numericInput("targets_company_id", "Company ID", value = 1, min = 1),
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
                     actionButton("add_target", "Add GHG Target"),
            ),
            
            
            ##########################################################################
            ###Tab 5### 
            tabPanel("Target Calculation",
                     
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
    input$emissions2020/input$output2020
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
      query <- "SELECT Company_Name FROM CP_MOTHERDATABASE"
      result <- dbGetQuery(pool, query)
      updateSelectInput(session, "companySelection", choices = c("Please select company name", result$Company_Name))
    } else {
      showNotification("Please log in to access the app.")
    }
  })
  
  #################################################################
  #Database input
  
  insert_data <- function(company_name, sector, hq_location, ca100, data_type, timestamp_data_entry, disclosure_date, year, ghg_scope, ghg_measure, ghg_activity, ghg_value, ghg_unit, ghg_type, activity_value, activity_unit, ghg_intensity) {
    max_id_query <- "CREATE TEMPORARY TABLE IF NOT EXISTS max_company_id (Max_ID VARCHAR(255));"
    dbExecute(pool, max_id_query)
    
    max_id_query <- sprintf("REPLACE INTO max_company_id (Max_ID) SELECT CONCAT('CID', LPAD(MAX(SUBSTRING(Company_ID, 4) + 1), 5, '0')) FROM CP_MOTHERDATABASE;")
    dbExecute(pool, max_id_query)
    
    query <- sprintf("INSERT INTO CP_MOTHERDATABASE (Company_Name, Sector, Company_ID, HQ_Location, CA100, Data_type, Timestamp_Data_Entry, Disclosure_Date, Year, GHG_Scope, GHG_Measure, GHG_Activity, GHG_Value, GHG_Unit, GHG_Type, Activity_Value, Activity_Unit, GHG_Intensity) VALUES ('%s', '%s', (SELECT Max_ID FROM max_company_id), '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')",
                     company_name, sector, hq_location, ca100, data_type, timestamp_data_entry, disclosure_date, year, ghg_scope, ghg_measure, ghg_activity, ghg_value, ghg_unit, ghg_type, activity_value, activity_unit, ghg_intensity)
    
    cat("Query: ", query, "\n") # Print the SQL query
    
    result <- NULL
    tryCatch({
      result <- dbExecute(pool, query)
    }, error = function(e) {
      cat("ERROR: ", conditionMessage(e), "\n")
    })
    return(result)
  }
  
  
  
  observeEvent(input$add_target, {
    dbExecute(pool, "INSERT INTO ghg_reduction_targets (company_id, target_name, target_year, target_value, target_unit, target_scope, base_year, target_status, disclosure_date, source_doc, source_page, source_date, target_comment)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
              params = list(input$targets_company_id, input$target_name, input$target_year, input$target_value, input$target_unit, input$target_scope, input$base_year, input$target_status, input$targets_disclosure_date, input$targets_source_doc, input$targets_source_page, input$targets_source_date, input$targets_comment))
    output$confirmation <- renderText("GHG reduction target added successfully.")
  })
  
  
  
  observeEvent(input$submitHistoricalData, {
    # Only insert data if a valid company is selected
    if (input$companySelection != "Please select company name") {
      result <- insert_data(
        company_name = input$companySelection, # Use the selected company instead of the new company
        sector = input$newCompanySector, # This field is not used when inserting historical data
        hq_location = input$newCompanyHQ, # This field is not used when inserting historical data
        ca100 = input$newCompanyCA100, # This field is not used when inserting historical data
        data_type = "Historical Data",
        timestamp_data_entry = Sys.time(),
        disclosure_date = input$disclosure_date,
        year = input$year,
        ghg_scope = input$ghg_scope,
        ghg_measure = input$ghg_measure,
        ghg_activity = 1,
        ghg_value = input$ab_emissions,
        ghg_unit = "tCO2e",
        ghg_type = "",
        activity_value = 1,
        activity_unit = "",
        ghg_intensity = 1
      )
      if (is.null(result)) {
        showNotification("Error adding historical data. Please check your input.", type = "error")
      } else {
        showNotification("Historical data added successfully.")
      }
    } else {
      showNotification("Please select a valid company before submitting historical data.", type = "error")
    }
  })
  
  
  observeEvent(input$addNewCompany, {
    result <- insert_data(
      company_name = input$newCompanyName,
      sector = input$newCompanySector,
      hq_location = input$newCompanyHQ,
      ca100 = input$newCompanyCA100,
      data_type = "", # Add a textInput for Data_type if needed
      timestamp_data_entry = Sys.time(), # Automatically get the current timestamp
      disclosure_date = "1900-01-01", # Add a dateInput for Disclosure_Date if needed
      year = "1990", # Add a numericInput for Year if needed
      ghg_scope = "", # Add a selectInput for GHG_Scope if needed
      ghg_measure = "", # Add a selectInput for GHG_Measure if needed
      ghg_activity = 1, # Add a textInput for GHG_Activity if needed
      ghg_value = 1, # Add a numericInput for GHG_Value if needed
      ghg_unit = "", # Add a selectInput for GHG_Unit if needed
      ghg_type = "", # Add a selectInput for GHG_Type if needed
      activity_value = 1, # Add a numericInput for Activity_Value if needed
      activity_unit = "", # Add a selectInput for Activity_Unit if needed
      ghg_intensity = 1 # Add a numericInput for GHG_Intensity if needed
    )
    if (is.null(result)) {
      showNotification("Error adding new company. Please check your input.", type = "error")
    } else {
      showNotification("New company added successfully.")
    }
  })
  
  
  observeEvent(reactive(credentials()$user_auth), {
    updateTextInput(session, "user_auth", value = credentials()$user_auth)
    if (credentials()$user_auth) {
      query <- "SELECT Company_Name FROM CP_MOTHERDATABASE"
      result <- dbGetQuery(pool, query)
      updateSelectInput(session, "companySelection", choices = c("Please select company name", result$Company_Name))
    }
  }, ignoreNULL = FALSE)
}

#################################################################
#Database output







shinyApp(ui = ui, server = server)