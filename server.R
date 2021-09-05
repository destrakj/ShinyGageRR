library(dplyr)
library(shinyscreenshot)

#server
shinyServer(function(input, output) {
  rawData <- eventReactive(input$csvFile, {
    na.omit(read.csv(
      input$csvFile$datapath,
      colClasses = c("factor", "factor", "factor", "numeric")
    ))
  })
  
  output$table <- renderTable({
    rawData()
  })
  
  output$with2 <- renderPrint({
    gagerr <- rawData()
    #ANOVA Two Way Without Interaction
    twoWayIn <- aov(Response ~ Part * Operator, data = gagerr)
    twoWayInSm <- summary(twoWayIn)
    rownames(twoWayInSm[[1]])[4] <- "Repeatability"
    twoWayInSm[[1]] <- rbind(twoWayInSm[[1]],
                             c(colSums(twoWayInSm[[1]][, 1:2]), rep(NA, 3)))
    rownames(twoWayInSm[[1]])[5] <- "Total"
    print(twoWayInSm)
    assign("twoWayInSum", data.frame(matrix(unlist(twoWayInSm[[1]]), nrow = 5, )), envir = .GlobalEnv)
  })
  
  output$with3 <- renderPrint({
    gagerr <- rawData()
    countPart <- gagerr %>% count(Other, Operator)
    countOperator <- gagerr %>% count(Other, Part)
    countResponse <- gagerr %>% count(Part, Operator)
    
    countPart <- countPart[1, 3]
    countOperator <- countOperator[1, 3]
    countResponse <- countResponse[1, 3]
    
    #Variance Components ANOVA Two Way Without Interaction
    varCompIn <- matrix(ncol = 2, nrow = 7)
    rownames(varCompIn) <- c(
      "Total Gage R&R",
      "  Repeatability",
      "  Reproducibility",
      "    Operator",
      "    Part : Operator",
      "Part-To-Part",
      "Total Variation"
    )
    colnames(varCompIn) <- c("varCompIn", "%Contrib")
    
    varCompIn[2, 1] <- twoWayInSum[4, 3]
    varCompIn[4, 1] <-
      (twoWayInSum[2, 3] - twoWayInSum[3, 3]) / (countPart * countResponse)
    varCompIn[5, 1] <-
      (twoWayInSum[3, 3] - twoWayInSum[4, 3]) / countResponse
    varCompIn[3, 1] <- (varCompIn[4, 1]) + (varCompIn[5, 1])
    varCompIn[1, 1] <- (varCompIn[2, 1]) + (varCompIn[3, 1])
    varCompIn[6, 1] <-
      (twoWayInSum[1, 3] - twoWayInSum[3, 3]) / (countOperator * countResponse)
    varCompIn[7, 1] <- varCompIn[1, 1] + varCompIn[6, 1]
    
    varCompIn[, 2] <-
      round(((varCompIn[, 1] / varCompIn[7, 1]) * 100), digits = 2)
    
    print(varCompIn)
    assign("varCompIn", data.frame(matrix(
      varCompIn, ncol = 2, nrow = 7,
    )), envir = .GlobalEnv)
  })
  
  output$with4 <- renderPrint({
    req(input$csvFile)
    gagEvalIn <- matrix(ncol = 4, nrow = 7)
    rownames(gagEvalIn) <- c(
      "Total Gage R&R",
      "  Repeatability",
      "  Reproducibility",
      "    Operator",
      "    Part : Operator",
      "Part-To-Part",
      "Total Variation"
    )
    colnames(gagEvalIn) <-
      c("StdDev", "StudyVar", "%StudyVar", "%Tolerance")
    
    gagEvalIn[, 1] <- round(sqrt(varCompIn[, 1]), digits = 3)
    gagEvalIn[, 2] <- gagEvalIn[, 1] * input$sigma
    gagEvalIn[, 3] <-
      round(((gagEvalIn[, 2] / gagEvalIn[7, 2]) * 100), digits = 2)
    gagEvalIn[, 4] <-
      round((gagEvalIn[, 2] / (input$usl - input$lsl) * 100), 2)
    
    print(gagEvalIn)
    assign("gagEvalIn", data.frame(matrix(
      gagEvalIn, ncol = 4, nrow = 7,
    )), envir = .GlobalEnv)
  })
  
  output$with5 <- renderPrint({
    req(input$csvFile)
    if (gagEvalIn[1, 3] > 30) {
      print(" %R&R Study : Unacceptable")
    } else if (gagEval[1, 3] <= 10) {
      print(" %R&R Study : Expected")
    } else if (gagEval[1, 3] <= 20) {
      print(" %R&R Study : Acceptable")
    } else if (gagEval[1, 3] <= 30) {
      print(" %R&R Study : Borderline")
    }
  })
  
  output$without2 <- renderPrint({
    gagerr <- rawData()
    #ANOVA Two Way Without Interaction
    twoWay <- aov(Response ~ Part + Operator, data = gagerr)
    twoWaySm <- summary(twoWay)
    rownames(twoWaySm[[1]])[3] <- "Repeatability"
    twoWaySm[[1]] <- rbind(twoWaySm[[1]],
                           c(colSums(twoWaySm[[1]][, 1:2]), rep(NA, 3)))
    rownames(twoWaySm[[1]])[4] <- "Total"
    print(twoWaySm)
    assign("twoWaySm", data.frame(matrix(unlist(twoWaySm[[1]]), nrow = 4, )), envir = .GlobalEnv)
  })
  
  output$without3 <- renderPrint({
    gagerr <- rawData()
    countPart <- gagerr %>% count(Other, Operator)
    countOperator <- gagerr %>% count(Other, Part)
    countResponse <- gagerr %>% count(Part, Operator)
    
    countPart <- countPart[1, 3]
    countOperator <- countOperator[1, 3]
    countResponse <- countResponse[1, 3]
    
    #Variance Components ANOVA Two Way Without Interaction
    varComp <- matrix(ncol = 2, nrow = 6)
    rownames(varComp) <- c(
      "Total Gage R&R",
      "  Repeatability",
      "  Reproducibility",
      "    Operator",
      "Part-To-Part",
      "Total Variation"
    )
    colnames(varComp) <- c("VarComp", "%Contrib")
    
    varComp[2, 1] <- twoWaySm[3, 3]
    varComp[4, 1] <-
      (twoWaySm[2, 3] - twoWaySm[3, 3]) / (countPart * countResponse)
    varComp[5, 1] <-
      (twoWaySm[1, 3] - twoWaySm[3, 3]) / (countOperator * countResponse)
    varComp[3, 1] <- varComp[4, 1]
    varComp[1, 1] <- (varComp[2, 1]) + (varComp[3, 1])
    varComp[6, 1] <- varComp[1, 1] + varComp[5, 1]
    
    varComp[, 2] <-
      round(((varComp[, 1] / varComp[6, 1]) * 100), digits = 2)
    
    print(varComp)
    assign("varComp", data.frame(matrix(
      varComp, ncol = 2, nrow = 6,
    )), envir = .GlobalEnv)
  })
  
  output$without4 <- renderPrint({
    req(input$csvFile)
    gagEval <- matrix(ncol = 4, nrow = 6)
    rownames(gagEval) <- c(
      "Total Gage R&R",
      "  Repeatability",
      "  Reproducibility",
      "    Operator",
      "Part-To-Part",
      "Total Variation"
    )
    colnames(gagEval) <-
      c("StdDev", "StudyVar", "%StudyVar", "%Tolerance")
    
    gagEval[, 1] <- round(sqrt(varComp[, 1]), digits = 3)
    gagEval[, 2] <- gagEval[, 1] * input$sigma
    gagEval[, 3] <-
      round(((gagEval[, 2] / gagEval[6, 2]) * 100), digits = 2)
    gagEval[, 4] <-
      round((gagEval[, 2] / (input$usl - input$lsl) * 100), 2)
    
    print(gagEval)
    assign("gagEval", data.frame(matrix(
      gagEval, ncol = 4, nrow = 6,
    )), envir = .GlobalEnv)
  })
  
  output$without5 <- renderPrint({
    req(input$csvFile)
    if (gagEval[1, 3] > 30) {
      print(" %R&R Study : Unacceptable")
    } else if (gagEval[1, 3] <= 10) {
      print(" %R&R Study : Expected")
    } else if (gagEval[1, 3] <= 20) {
      print(" %R&R Study : Acceptable")
    } else if (gagEval[1, 3] <= 30) {
      print(" %R&R Study : Borderline")
    }
  })
  
  observeEvent(input$downloadData, {
    screenshot()
  })
})