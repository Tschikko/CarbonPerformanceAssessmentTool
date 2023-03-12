library("shiny")
library("DT")

setwd("/Users/AMINA3/OneDrive - London School of Economics/CP data lead/R-shiny portal/")
AAAfile <- read.csv("Sample data.csv")
colnames(AAAfile) <- c("Company Name", "Sector", "TPI ID", "Headquarter Location", 
                       "Company Size", "CA100", "Latest year of disclosure", "Emissions - tonnes of CO2", "Activity - MWh",
                       "Emissions Intensity")
row.names(AAAfile) <- AAAfile$`Company Name`

ui <- fluidPage(
  ###Title
  titlePanel("Carbon Perfomance"),
  
  sidebarLayout(
    sidebarPanel((""),
                 ###>>>Company select
                 selectInput("company", "Pick a company to perform a Carbon Performance:", 
                             choices = c("Please select company name",AAAfile$`Company Name`)),
                 submitButton("See company details"),
                 
                 ##>>>Emissions Data Input
                 
                 selectInput("emissionsAccountingBoundary2020", "Select accounting boundary on which emissions are stated",
                             c("Equity" = "equity",
                               "Operational" = "operational",
                               "Unknown"="unkown")),
                 
                 
                 numericInput("yearofdisclosure","Please enter year of dislcosure",0000,2050),  
                 
                 textOutput("emissionsDataCollection"),
                 numericInput("emissions2020","Emissions 2020", 0, 10000000000000000),
                 
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
                 
                 #fileInput("additionalIntensityCalculations", "Please upload any files including additional permutations or modifications made to disclosed values. Please follow the format you can find here."),
                 
                 
                 ###>>>Company intensity Data Input
                 textOutput("intensityDataCollection"),
                 numericInput("intensity2020","Intensity 2020", 0, 10000000000000000),
                 selectInput("intensityAccountingBoundary2020", "Select accounting boundary on which the emissions intensity is stated",
                             c("Equity" = "equity",
                               "Operational" = "operational")),
                 textInput("intensitySource2020", "Please provide the name of the source document"),
                 numericInput("intensitySourcePage2020","Please provide the source page",0, 10000000000000000),
                 textInput("intensityComments2020", "Please provide any further comments including sources where appropriate")),
    
    #output telling you difference between calculated and reported intensity
    
    
    
    
    # submitButton("Submit", icon("refresh")),
    # 
    # 
    # textOutput("emissionsIntensity"),
    # 
    # radioButtons("disclosureVSownestimate", "Is the disclosed intensity 
    #              substantiated by own estimated intensity?", c("Yes"="yes", "No"="no"),selected ="no"),
    # 
    # 
    # plotOutput("carbonPerformance")),
    
    
    
    
    
    
    mainPanel(h3("Company details"),
              tableOutput("companydetails"))
  )  
)


#   
server <- function(input, output){
  
  
  
  output$companydetails <- renderTable({
    AAAfile[input$company,]
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