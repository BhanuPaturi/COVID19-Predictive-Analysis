

server <- function(input, output) {
  onSessionEnded(function() {
    stopApp()
  })
  
  getData <- reactive({
    covidData <- read.csv("Ass2Data.csv",
                          header = TRUE,
                          na.strings = c("--","-99","NA"),
                          stringsAsFactors = TRUE)
    set.seed(193)
    covidData[covidData == ""] <- NA
    covidData
  })
  
  
  #############Splitting the data
  
  getTrainData<- reactive({
    data <- getCleanData2()
    d <- data[data$OBS_TYPE == "Train",]
  })
  
  getTestData<- reactive({
    Testdata <-setdiff(getCleanData2(),getTrainData())
  })
  
  output$rowsColumns<-renderPrint({
    data <- getBakedRecipe()
    data1<- getTrainData()
    data2<- getTestData()
    print (paste("Transformed Data has ", nrow(data), " rows and ", ncol(data), " columns"))
    print (paste("Train Data has ", nrow(data1), " rows and ", ncol(data1), " columns"))
    print (paste("Test Data has ", nrow(data2), " rows and ", ncol(data2), " columns"))
   
  })
  output$tblTransformedData <- DT::renderDataTable({
     DT::datatable(getBakedRecipe())
  })
  
 
  
  #######missing Correlation Chart
  output$MissingCor <- renderPlot({
    data <- getData()
    req(data)
    vis_miss(data)
    })
    #naniar::gg_miss_upset(data, nsets = 6)
  #})
 #grid.text("Correlation chart",x = 0.65, y=0.95, gp=gpar(fontsize=20))
   
  
  ##### Variable Importance
  output$VarImp <- renderPlot({
    data <- getData()
    req(data)
    rfB <- caret::train(DEATH_RATE ~.-CODE-OBS_TYPE, data = data, method = "rf", na.action=na.exclude) 
    varImpObj <- varImp(rfB)
    plot(varImpObj, main = "Variable Importance", top = 10)
  })
  
  ##### predicting missingness
  
  output$PredMissing <- renderPlot({
    data <- getData()
    data$Missingness <- apply(X = is.na(data), MARGIN = 1, FUN = sum)
    tree <- train(Missingness ~ .-CODE-OBS_TYPE, data = data, method = "rpart", na.action = na.rpart)
    rpart.plot(tree$finalModel, main = "", roundint = TRUE, clip.facs = TRUE)
  })
  
  getBakedRecipe <- reactive({
    recPrep <- prep(getRecipe())
    bake(recPrep, new_data = NULL)
  })
  
  output$Summary <- renderPrint({
    summary(getBakedRecipe())
  })
  
 ##### To Remove Missingness  
  output$Missing <- renderPlot({
    visdat::vis_miss(getCleanData2()) +
      labs(title = paste("Thresholds VarMiss:", input$VarThreshold, "ObsMiss:", input$ObsThreshold))
  })
  
  getCleanData <- reactive({
    data <-getData()
    if(input$chkHealthCost == TRUE){
      data$HEALTHCARE_COST[data$HEALTHCARE_BASIS == "FREE" & is.na(data$HEALTHCARE_COST)] <- 0
    }
    data
  })
  
  output$MissingClean <- renderPlot({
    visdat::vis_miss(getData()) +
      labs(title = paste("Thresholds VarMiss:", input$VarThreshold, "ObsMiss:", input$ObsThreshold))
  })
  
  
  getCleanData1 <- reactive({
    # remove excessively missing Vars
    data <-  getCleanData()
    vRatio <- apply(X = data, MARGIN = 2, FUN = pMiss)
    data[, vRatio < input$VarThreshold]
  })  
  
  getCleanData2 <- reactive({
    # remove excessively missing Obs
    data <- getCleanData1()
    oRatio <- apply(X = data, MARGIN = 1, FUN = pMiss)
    data[oRatio < input$ObsThreshold, ]
  })  
  
  
  output$Missing <- renderPlot({
    visdat::vis_miss(getCleanData2()) +
      labs(title = paste("Thresholds VarMiss:", input$VarThreshold, "ObsMiss:", input$ObsThreshold))
  })
  
  getRecipe <- reactive({
    rec <- recipe(DEATH_RATE ~ ., getCleanData2())
    rec <- update_role(rec, CODE, new_role = "id")
    rec <- update_role(rec, OBS_TYPE, new_role = "Split")
    if (input$center == TRUE) { # if centering and scaling requested
      rec <- step_center(rec, all_numeric_predictors())
    }
    if (input$scale == TRUE) {
      rec <- step_scale(rec, all_numeric_predictors())
    }
    if (input$ShadowVar == TRUE) {
      rec <- step_indicate_na(rec, all_predictors())
    }
    if (input$ImpMethod == "KNN"){
      rec <- step_impute_knn(rec, all_numeric_predictors(),  neighbors = input$sliderKNN)
    } else if (input$ImpMethod == "Median") {
      rec <- step_impute_median(rec, all_numeric_predictors())
    } else if (input$ImpMethod == "Partial Del") {
      rec <- step_naomit(rec, all_numeric_predictors(), skip = TRUE)
    }
    if (input$selectNom == "KNN") {
      rec <- step_impute_knn(rec, all_nominal_predictors(), neighbors = input$sliderKNN)
    } else if (input$selectNom == "Mode") {
      rec <- step_impute_mode(rec, all_nominal_predictors())
    } else if (input$selectNom == "Partial Del") {
      rec <- step_naomit(rec, all_nominal_predictors(), skip = TRUE)
    }
    rec <- step_dummy(rec, all_nominal_predictors())
    rec <- step_zv(rec, all_predictors())
    rec
  })  
  
  output$ImputationMissing <-  renderPlot({
    visdat::vis_miss(getCleanData2()) +
      labs(title = paste("Thresholds VarMiss:", input$VarThreshold, "ObsMiss:", input$ObsThreshold))
  })
  
  
  output$RecipeTable <- renderPrint({
    print(getRecipe())
  })
  
  output$RecipeSummary <- renderPrint({
    summary(getRecipe())
  })
  
  getModel <- reactive({
    model <- caret::train(getRecipe(),
                          data = getTrainData(),
                          method = "glmnet",
                          metric = "RMSE",
                          trControl = trControl,
                          tuneGrid = expand.grid(alpha = seq(0,1,0.1), lambda = seq(0.1, 10, by = 0.1)))
    model
  })
  
  output$glmnetModelSummary1 <- renderTable({
       model <- getModel()
       req(model)
       as.data.frame(model$bestTune)
     })
  output$glmnetModelPlots <- renderPlot({
       model <- getModel()
       req(model)
       plot(model$finalModel)
     })
  output$glmnetModelSummary2 <- renderTable({
    model <- getModel()
    req(model)
    print(model)
    })
  
  predictModel <- reactive({
    model <- getModel()
    testData <- getTestData()
    req(model)
    predict(model, newdata = testData)
  })
  
  getTestResults <- reactive({
    data <- getTestData()
    predicted <- predictModel()
    df <- data.frame(data$DEATH_RATE, predicted)
    colnames(df) <- c("obs", "pred")
    df
  })

  output$testPlot <- renderPlot({
    plot(getTestResults(), main = "Predicted versus Observed", col = "red", pch = 19, cex = 1.2)
    abline(a = 0, b = 1, col = "blue")
  })
  output$testSummary <- renderPrint({
    print(getTestResults())
  })
 
  
  # getResidualRecipe <- reactive({
  #   rec <- recipe(DEATH_RATE ~ ., getTestData)
  #   rec<- step_naomit(everything())
  #   rec
  # })
  # getBakedRecipe1 <- reactive({
  #   recPrep <- prep(getResidualRecipe())
  #   bake(recPrep, new_data = NULL)
  # })
  # getResidualModel <- reactive({
  #   model <- randomForest::randomForest(DEATH_RATE ~ all_numeric_predictors(), getBakedRecipe1())
  #   model
  # })
  # 
  # getResiduals <- reactive({
  #   data <- getBakedRecipe1()
  #   residuals <- data$DEATH_RATE - predict(genModel, newdata = data)
  # })
  # 
  # getResidualDf <- reactive({
  # 
  #   data < getResiduals()
  #   req(data)
  #   names(data) <- rownames(getBakedRecipe1())
  #   sd(data)
  #   coef <- 2
  #   limits <- boxplot.stats(x = data, coef = coef)$stats
  #   label <- ifelse(data < limits[1] | data > limits[5], names(data), NA)
  #   df <- data.frame(residuals, label)
  # })
  # 
  # 
  # 
  # output$residualBoxPlot <- renderPlot({
  #   data <- getResidualDf()
  #   residuals <- getResiduals()
  #   req(data)
  #   ggplot(df, mapping = aes(x = residuals), y = 0) +
  #     geom_boxplot(coef = coef, outlier.colour = "red") +
  #     ggrepel::geom_text_repel(max.overlaps = 50, mapping = aes(label = label)) +
  #     labs(title = paste("Boxplot using", coef, "as IQR Multiplier")) +
  #     theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  # })
  
}