library(dplyr)
library(ggplot2)
library(gridExtra)
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
    if(varCompIn[4, 1] < 0){
      varCompIn[4, 1] = 0
    }
    varCompIn[5, 1] <-
      (twoWayInSum[3, 3] - twoWayInSum[4, 3]) / countResponse
    if(varCompIn[5, 1] < 0){
      varCompIn[5, 1] = 0
    }
    varCompIn[3, 1] <- (varCompIn[4, 1]) + (varCompIn[5, 1])
    varCompIn[1, 1] <- (varCompIn[2, 1]) + (varCompIn[3, 1])
    varCompIn[6, 1] <-
      (twoWayInSum[1, 3] - twoWayInSum[3, 3]) / (countOperator * countResponse)
    varCompIn[7, 1] <- varCompIn[1, 1] + varCompIn[6, 1]
    
    varCompIn[, 2] <-
      round(((varCompIn[, 1] / varCompIn[7, 1]) * 100), digits = 8)
    
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
    
    gagEvalIn[, 1] <- round(sqrt(varCompIn[, 1]), digits = 8)
    gagEvalIn[, 2] <- gagEvalIn[, 1] * input$sigma
    gagEvalIn[, 3] <-
      round(((gagEvalIn[, 2] / gagEvalIn[7, 2]) * 100), digits = 8)
    gagEvalIn[, 4] <-
      round((gagEvalIn[, 2] / (input$usl - input$lsl) * 100), 8)
    
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
  
  output$with6 <- renderPlot({
    req(input$csvFile)
    gagerr <- rawData()
    covInPercent <-
      matrix(c(varCompIn[1:3, 2], varCompIn[6, 2], gagEvalIn[1:3, 3], gagEvalIn[6, 3]))
    covInXaxis <-
      rep(c("Gage R&R", "Repeat", "Reprod", "Part-To-Part"), 2)
    covInFill <- c(rep("%Contrib", 4), rep("%StudyVar", 4))
    covInData <- data.frame(covInPercent, covInXaxis, covInFill)
    
    GrrcovIn <-
      ggplot(covInData,
             aes(fill = covInFill, y = covInPercent, x = covInXaxis)) +
      geom_bar(position = "dodge", stat = "identity") + ggtitle("Components of Variation") +
      labs(fill = "", x = "", y = "Percent") +
      scale_fill_manual(values = c("black", "dodgerblue")) +
      theme_update(plot.title = element_text(hjust = 0.5))
    GrrcovIn
    
    rbo <-
      ggplot(gagerr, aes(x = as.factor(Operator), y = Response)) +
      geom_boxplot() + ggtitle("Response by Operator") + labs(fill = "", x = "Operator", y = "Measurements") +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 18,
        size = 3,
        color = "red"
      ) +
      stat_summary(fun = mean,
                   geom = "line",
                   aes(group = 1),
                   color = "red")
    #stat_summary(fun = mean, geom="text", vjust=-0.25, aes( label=(..y..)))
    rbo
    
    XbarChartByOpMean <-
      aggregate(Response ~ Part + Operator, data = gagerr, mean)
    XbarChartByOp <-
      XbarChartByOpMean[order(as.numeric(as.character(XbarChartByOpMean$Part))), ]
    
    XbarChartByOp <-
      ggplot(XbarChartByOp,
             aes(
               x = as.numeric(as.character(Part)),
               y = Response,
               color =  ,
               group = 1
             )) +
      geom_point() + geom_path() + ggtitle("Xbar Chart by Operator") + labs(fill = "", x = "Part", y = "Sample Mean") +
      facet_wrap(~ Operator)
    theme_update(plot.title = element_text(hjust = 0.5))
    XbarChartByOp
    
    SubsetOp1 <- subset(gagerr, Operator == 1)
    SubsetOp1Ot1 <-
      subset(SubsetOp1, Other == 1, select = c(Part, Operator, Response))
    SubsetOp1Ot2 <-
      subset(SubsetOp1, Other == 2, select = c(Part, Operator, Response))
    SubsetMergeOp1 <- merge(SubsetOp1Ot1, SubsetOp1Ot2, "Part")
    SubsetMergeOp1 <-
      SubsetMergeOp1[order(as.numeric(as.character(SubsetMergeOp1$Part))), ]
    DfSubsetOp1 <-
      data.frame(
        SubsetMergeOp1$Part,
        SubsetMergeOp1$Operator.x,
        abs(SubsetMergeOp1$Response.x - SubsetMergeOp1$Response.y)
      )
    colnames(DfSubsetOp1) <- c("Part", "Operator", "Response")
    
    SubsetOp2 <- subset(gagerr, Operator == 2)
    SubsetOp2Ot1 <-
      subset(SubsetOp2, Other == 1, select = c(Part, Operator, Response))
    SubsetOp2Ot2 <-
      subset(SubsetOp2, Other == 2, select = c(Part, Operator, Response))
    SubsetMergeOp2 <- merge(SubsetOp2Ot1, SubsetOp2Ot2, "Part")
    SubsetMergeOp2 <-
      SubsetMergeOp2[order(as.numeric(as.character(SubsetMergeOp2$Part))), ]
    DfSubsetOp2 <-
      data.frame(
        SubsetMergeOp2$Part,
        SubsetMergeOp2$Operator.x,
        abs(SubsetMergeOp2$Response.x - SubsetMergeOp2$Response.y)
      )
    colnames(DfSubsetOp2) <- c("Part", "Operator", "Response")
    
    RChartSubsetByOp <- rbind(DfSubsetOp1, DfSubsetOp2)
    
    RChartByOp <-
      ggplot(RChartSubsetByOp, aes(
        x = as.numeric(as.character(Part)),
        y = Response,
        group = 1
      )) +
      geom_point() + geom_path() + ggtitle("R Chart by Operator") + labs(fill = "", x = "Part", y = "Sample Range") +
      facet_wrap(~ Operator)
    theme_update(plot.title = element_text(hjust = 0.5))
    RChartByOp
    
    grid.arrange(
      arrangeGrob(GrrcovIn, rbo, ncol = 2),
      arrangeGrob(XbarChartByOp, RChartByOp, ncol = 2),
      nrow = 2
    )
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
    if(varComp[4, 1] < 0){
      varComp[4, 1] = 0
    }
    varComp[5, 1] <-
      (twoWaySm[1, 3] - twoWaySm[3, 3]) / (countOperator * countResponse)
    varComp[3, 1] <- varComp[4, 1]
    varComp[1, 1] <- (varComp[2, 1]) + (varComp[3, 1])
    varComp[6, 1] <- varComp[1, 1] + varComp[5, 1]
    
    varComp[, 2] <-
      round(((varComp[, 1] / varComp[6, 1]) * 100), digits = 8)
    
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
    
    gagEval[, 1] <- round(sqrt(varComp[, 1]), digits = 8)
    gagEval[, 2] <- gagEval[, 1] * input$sigma
    gagEval[, 3] <-
      round(((gagEval[, 2] / gagEval[6, 2]) * 100), digits = 8)
    gagEval[, 4] <-
      round((gagEval[, 2] / (input$usl - input$lsl) * 100), 8)
    
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
  
  output$without6 <- renderPlot({
    req(input$csvFile)
    gagerr <- rawData()
    covPercent <-
      matrix(c(varComp[1:3, 2], varComp[5, 2], gagEval[1:3, 3], gagEval[5, 3]))
    covXaxis <-
      rep(c("Gage R&R", "Repeat", "Reprod", "Part-To-Part"), 2)
    covFill <- c(rep("%Contrib", 4), rep("%StudyVar", 4))
    covData <- data.frame(covPercent, covXaxis, covFill)
    
    Grrcov <-
      ggplot(covData, aes(fill = covFill, y = covPercent, x = covXaxis)) +
      geom_bar(position = "dodge", stat = "identity") + ggtitle("Components of Variation") +
      labs(fill = "", x = "", y = "Percent") +
      scale_fill_manual(values = c("black", "dodgerblue")) +
      theme_update(plot.title = element_text(hjust = 0.5))
    Grrcov
    
    rbo <-
      ggplot(gagerr, aes(x = as.factor(Operator), y = Response)) +
      geom_boxplot() + ggtitle("Response by Operator") + labs(fill = "", x = "Operator", y = "Measurements") +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 18,
        size = 3,
        color = "red"
      ) +
      stat_summary(fun = mean,
                   geom = "line",
                   aes(group = 1),
                   color = "red")
    #stat_summary(fun = mean, geom="text", vjust=-0.25, aes( label=(..y..)))
    rbo
    
    XbarChartByOpMean <-
      aggregate(Response ~ Part + Operator, data = gagerr, mean)
    XbarChartByOp <-
      XbarChartByOpMean[order(as.numeric(as.character(XbarChartByOpMean$Part))), ]
    
    XbarChartByOp <-
      ggplot(XbarChartByOp, aes(
        x = as.numeric(as.character(Part)),
        y = Response,
        group = 1
      )) +
      geom_point() + geom_path() + ggtitle("Xbar Chart by Operator") + labs(fill = "", x = "Part", y = "Sample Mean") +
      facet_wrap(~ Operator)
    theme_update(plot.title = element_text(hjust = 0.5))
    XbarChartByOp
    
    SubsetOp1 <- subset(gagerr, Operator == 1)
    SubsetOp1Ot1 <-
      subset(SubsetOp1, Other == 1, select = c(Part, Operator, Response))
    SubsetOp1Ot2 <-
      subset(SubsetOp1, Other == 2, select = c(Part, Operator, Response))
    SubsetMergeOp1 <- merge(SubsetOp1Ot1, SubsetOp1Ot2, "Part")
    SubsetMergeOp1 <-
      SubsetMergeOp1[order(as.numeric(as.character(SubsetMergeOp1$Part))), ]
    DfSubsetOp1 <-
      data.frame(
        SubsetMergeOp1$Part,
        SubsetMergeOp1$Operator.x,
        abs(SubsetMergeOp1$Response.x - SubsetMergeOp1$Response.y)
      )
    colnames(DfSubsetOp1) <- c("Part", "Operator", "Response")
    
    SubsetOp2 <- subset(gagerr, Operator == 2)
    SubsetOp2Ot1 <-
      subset(SubsetOp2, Other == 1, select = c(Part, Operator, Response))
    SubsetOp2Ot2 <-
      subset(SubsetOp2, Other == 2, select = c(Part, Operator, Response))
    SubsetMergeOp2 <- merge(SubsetOp2Ot1, SubsetOp2Ot2, "Part")
    SubsetMergeOp2 <-
      SubsetMergeOp2[order(as.numeric(as.character(SubsetMergeOp2$Part))), ]
    DfSubsetOp2 <-
      data.frame(
        SubsetMergeOp2$Part,
        SubsetMergeOp2$Operator.x,
        abs(SubsetMergeOp2$Response.x - SubsetMergeOp2$Response.y)
      )
    colnames(DfSubsetOp2) <- c("Part", "Operator", "Response")
    
    RChartSubsetByOp <- rbind(DfSubsetOp1, DfSubsetOp2)
    
    RChartByOp <-
      ggplot(RChartSubsetByOp, aes(
        x = as.numeric(as.character(Part)),
        y = Response,
        group = 1
      )) +
      geom_point() + geom_path() + ggtitle("R Chart by Operator") + labs(fill = "", x = "Part", y = "Sample Range") +
      facet_wrap(~ Operator)
    theme_update(plot.title = element_text(hjust = 0.5))
    RChartByOp
    
    grid.arrange(
      arrangeGrob(Grrcov, rbo, ncol = 2),
      arrangeGrob(XbarChartByOp, RChartByOp, ncol = 2),
      nrow = 2
    )
  })
  
  observeEvent(input$downloadData, {
    screenshot()
  })
})
