
ui <- fluidPage(
  titlePanel("Assignment 2 - Bhanu"),
  shinyjs::useShinyjs(),
  tabsetPanel(
    tabPanel("Strategy",
             h3("Preprocessing"),
             tabsetPanel(
              tabPanel("Missing Pattern",
                       sidebarLayout(
                         sidebarPanel(
                           checkboxInput(inputId = "scale", label = "Scaling", value = FALSE),
                           checkboxInput(inputId = "center", label = "Centring", value = FALSE),
                           width = 3),
                         mainPanel(
                        withSpinner(
                          plotOutput(outputId = "MissingCor")
                        )))),
               tabPanel("Predicting Missingness",
                        checkboxInput(inputId = "ShadowVar", label = "Add shadow variables"),
                        withSpinner(
                          plotOutput(outputId = "PredMissing")
                        ),
                        withSpinner(
                          verbatimTextOutput(outputId = "Summary")
                        )),
               tabPanel("Variable Importance",
                        withSpinner(
                          plotOutput(outputId = "VarImp")
                        )),
               tabPanel("Missing",
                        sidebarLayout(
                          sidebarPanel(
                            checkboxInput("chkHealthCost", "0 when HealthCare_Basis is FREE and HealthCost is NA", value = FALSE),
                            sliderInput(inputId = "VarThreshold", label = "Threshold of variable missingness", 
                                        min = 1, max = 100, value = 100, post = "%"),
                            sliderInput(inputId = "ObsThreshold", label = "Threshold of observations missingness", 
                                        min = 1, max = 100, value = 100, post = "%")),
                          mainPanel(
                            
                            withSpinner(
                              plotOutput(outputId = "Missing")
                            )))),
               tabPanel("Imputation",
                        sidebarLayout(
                          sidebarPanel(  
                            selectInput(inputId = "ImpMethod", label = "Imputation method for Numeric variables", 
                                        choices = c("None", "KNN", "Partial Del","Median"), selected = "KNN"),
                            selectInput(inputId = "selectNom", label = "Imputation method for Nominal variables",
                                        choices = c("None", "KNN", "Partial Del","Mode"), selected = "KNN"),
                            sliderInput(inputId = "sliderKNN", label = "KNN", 
                                        min = 1, max = 10, value = 5)
                          ),
                          mainPanel(
                            withSpinner(
                              plotOutput(outputId = "ImputationMissing")
                            ),
                          ))),
               
               tabPanel("Summary",
                        withSpinner(
                          verbatimTextOutput(outputId = "RecipeTable")
                        ),
                        withSpinner(
                          verbatimTextOutput(outputId = "RecipeSummary")
                        )))),
    tabPanel("Model",
             h3("Predictive analytics for mortality in patients with COVID-19"),
             tabsetPanel(
               tabPanel("Split Data",
                        h2("Transformed data"),
                        withSpinner(
                          verbatimTextOutput("rowsColumns")
                        ),
                        withSpinner(
                          DT::dataTableOutput("tblTransformedData")
                        )
               ),
               tabPanel("Modelling",
                        withSpinner(
                          tableOutput("glmnetModelSummary2"),
                        )),
               tabPanel("Hyper parameters",
                        withSpinner(
                          plotOutput(outputId = "glmnetModelPlots")
                        ),
                        withSpinner(
                          tableOutput(outputId = "glmnetModelSummary1") 
                        )),
               tabPanel("Predicting",
                         withSpinner(
                           plotOutput(outputId = "testPlot")
                         ),
                         withSpinner(
                         verbatimTextOutput(outputId = "testSummary")
                        )),
               # tabPanel("Outlier",
               #          selectInput(inputId = "selectDataType", label = "Select Dataset",
               #                      choices = c("Both", "Train", "Test"), selected = "Both"),
               #          sliderInput(inputId = "multiplier", label = "IQR multiplier", 
               #                      min = 1, max = 6, value = 1.5, post = "%"),
               #          withSpinner(
               #            plotOutput(outputId = "residualBoxPlot")
               #          )
               #)
                        ))
             ))
    
    
    
  
