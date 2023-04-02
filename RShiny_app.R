library("shiny")
library("DT")

#setwd("/Users/AMINA3/OneDrive - London School of Economics/CP data lead/R-shiny portal/")
#setwd('/Users/valentin/Library/CloudStorage/OneDrive-Personal/CP_Rshiny/CarbonPerformanceAssessmentData/Sample_Data.csv')
AAAfile <- read.csv("Sample_Data.csv")
colnames(AAAfile) <- c("Company_Name","Sector","Company_ID","HQ_Location","Company_Size","CA100+","Research_Date","Year","GHG_Value","GHG_Unit","Activity_Value","Activity_Unit","GHG_Intensity")
row.names(AAAfile) <- AAAfile$`Company Name`

ui <- fluidPage(
  ###Title
  titlePanel("Carbon Perfomance"),
  
  tabsetPanel( 
    h1("Update existing company"),

    tabPanel("Company Selection", 
    selectInput("companySelection", "Pick a company to perform a Carbon Performance assessment for:", 
                choices = c("Please select company name",AAAfile$Company_Name)),
    selectInput("companyAssessmentDate", "Please select the assessment date for which you would like to see data. Please note, once you submit new data, a new assessment date will appear below",
                choices = c("Please select date",AAAfile$Research_Date )),
    submitButton("See company details"),
    
    
    h1("Add new company"),
    textOutput("newCompanyIntro"),
    
    textInput("newCompanyName", "Please enter the name of the company you would like to add"),
    textInput("newCompanyID", "Please enter the ID of the company you would like to add"),
    selectInput("newCompanySector", "Please select the sector of the company you would like to add",
                choices = c("Please select sector",AAAfile$Sector))
    ),
    
    
    tabPanel("Company Pathway",
    
    plotOutput('CP_Pathway'),
    tableOutput('CP_Table')
    
    ),
    
    tabPanel("Historical Data", h1("Coming soon")),
    
    
    
    tabPanel("Historical Calculation",
             ###Company Details
             
             #tabsetpanels?
             
             
             ###>>>Emissions Data Input
             h1("Absolute data"),
             textOutput("emissionsDataCollection"),
             numericInput("emissions2020","Emissions 2020", 0, 10000000000000000),
             selectInput("emissionsAccountingBoundary2020", "Select accounting boundary on which emissions are stated",
                         c("Equity" = "equity",
                           "Operational" = "operational",
                           "Unknown"="unkown")),
             textInput("emissionsSource2020", "Please provide the name of the source document"),
             numericInput("emissionsSourcePage2020","Please provide the source page",0, 10000000000000000),
             dateInput("emissionsSourceDate", "Source publication date:", value = "2022-06-01", format = "mm/dd/yy"),
             ### Source URL
             textAreaInput("emissionsComments2020", "Please provide any further comments including sources where appropriate"),
             
             ###>>>Company output Data Input 
             textOutput("outputDataCollection"),
             numericInput("output2020","Output 2020", 0, 10000000000000000),
             selectInput("outputAccountingBoundary2020", "Select accounting boundary on which the output is stated",
                         c("Equity" = "equity",
                           "Operational" = "operational")),
             textInput("outputSource2020", "Please provide the name of the source document"),
             numericInput("outputSourcePage2020","Please provide the source page",0, 10000000000000000),
             textInput("outputComments2020", "Please provide any further comments including sources where appropriate"),
             
             fileInput("additionalIntensityCalculations", "Please upload any files including additional permutations or modifications made to disclosed values. Please follow the format you can find here."),
             
             h1("Own intensity calculation"),
             submitButton("Submit", icon("refresh")),
             
             
             textOutput("emissionsIntensity"),
             
             
             
             h1("Intensity data"),
             ###>>>Company intensity Data Input 
             textOutput("intensityDataCollection"),
             numericInput("intensity2020","Intensity 2020", 0, 10000000000000000),
             selectInput("intensityAccountingBoundary2020", "Select accounting boundary on which the emissions intensity is stated",
                         c("Equity" = "equity",
                           "Operational" = "operational")),
             textInput("intensitySource2020", "Please provide the name of the source document"),
             numericInput("intensitySourcePage2020","Please provide the source page",0, 10000000000000000),
             textInput("intensityComments2020", "Please provide any further comments including sources where appropriate"),
             
             #output telling you difference between calculated and reported intensity
             
             radioButtons("disclosureVSownestimate", "Is the disclosed intensity substantiated by own estimated intensity?", c("Yes"="yes", "No"="no"),selected ="no"),
             
             
             plotOutput("carbonPerformance")
    ),
    
    tabPanel("Target Data"),
    
    h1("Absolute data"),
    
    h1("Intensity data"),
    
    tabPanel("Target Calculation"),
    
    h1("Intensity target"),
    
    h1("Absolute target"),
    
             
  )
)




server <- function(input, output, session){
  output$newCompanyIntro <- renderText({
    c("If the company you are researching is not yet covered, please create an entry below:")
  })
  output$companyDetails <- renderText({
    c("Company Name: SSE >", "Sector: Electricity Sector>", "ID: 1>", "Headquarter Location: UK>", "Company Size: Large>, CA100+ Focus Company: Yes")
  })
  output$emissionsDataCollection <- renderText({
    c("When collecting emissions data please ensure the data only reflects scope 1 emissions from the company's owned electricity generation, exlcuding any emissions from non-electricity generation related activities (e.g. such as from a gas distribution business run by the same company) or emissions relating to electricity purchased by the company for the sole purpse of re-selling it. For more information on emission scopes please click here. Key source documents usually are: Annual reports (or 10-K's/20F's in the US), Sustainability reports or similar, Climate specific reports (e.g. climate roadmap), Transition reports, investor presentations, websites and CDP responses. In the US please consult IEEE ESG templates as a key source if provided by the company.")
  })
  output$outputDataCollection <- renderText({
    c("When collecting output data please ensure the data only reflects the company's owned electricity generation stated on the same accounting boundary as the emissions data, exlcuding electricity purchased by the company for the sole purpse of re-selling it.")
  })
  output$emissionsIntensity <- reactive({
    input$emissions2020/input$output2020
  })
}

shinyApp(ui = ui, server = server)








