library(shinyjs)
library(rhandsontable)
source("./controllers.R")


server = function(input, output, session) {

  useShinyjs()
  
  pageNo = reactiveVal(1)
  
  maxPageNo = reactiveVal(NULL)

  stateListGlobal = NULL

  fileToMatrixPR = reactive({
    if (is.null(input$fileInputPR)) return(NULL)
    fileToRead = input$fileInputPR
    df = read.csv(input$fileInputPR$datapath, header=input$headerCheckPR, sep=",", quote="")
    return(as.matrix(df, nrow = 1, ncol = 1))
  })
  
  output$fileContentsPR = renderTable({
    if (is.null(fileToMatrixPR())) return(NULL)
    vectorNames = c("x", "y")
    matrixOutput = fileToMatrixPR()
    updateSliderInput(session, 
      "degreeNPR", 
      label = "Degree",
      min = 1, 
      max = length(matrixOutput[, 1]) - 1,
      step = 1,
      value=0
    )
    
    colnames(matrixOutput) = vectorNames
    if(input$sortedXPR) {
      matrixOutput = matrixOutput[order(matrixOutput[,1]), ]
    }
    if(!input$dispAllPR)  {
      return(head(matrixOutput))
    } else {
      return(matrixOutput)
    }
    
  })
  
  getFunction = eventReactive(input$solveButtonPR, {
    if (is.null(fileToMatrixPR())) return(NULL)
    matrixHandler = fileToMatrixPR()
    result = PolynomialRegression(matrixHandler[,1], matrixHandler[,2], input$degreeNPR)
    show("funcLabel")
    show("xInputPR")
    show("solveXPR")
    return(result)
  })
  
  getFunctionText = reactive({
    if(is.null(getFunction())) return(NULL)
    functionText = getFunction()$textForm
    return(functionText)
  })
  
  output$answerFunctionPR = renderText({
    getFunctionText()
  })

  output$answerGivenX = eventReactive(input$solveXPR, {
    if (is.null(fileToMatrixPR())) return(NULL)
    show("ansLabel")
    round(PRSolver(fileToMatrixPR()[,1], fileToMatrixPR()[,2], input$degreeNPR, input$xInputPR), digits=4)
  })
  
  fileToMatrixQSI = reactive({
    if (is.null(input$fileInputQSI)) return(NULL)
    fileToRead = input$fileInputQSI
    df = read.csv(fileToRead$datapath, header=input$headerCheckQSI, sep=",", quote="")
    toReturn = as.matrix(df, nrow = 1, ncol = 1)
    return(toReturn)
  })
  
  output$fileContentsQSI = renderTable({
    if (is.null(fileToMatrixQSI())) return(NULL)
    vectorNames = c("x", "y")
    matrixOutput = fileToMatrixQSI()
    colnames(matrixOutput) = vectorNames
    if(input$sortedXQSI) {
      matrixOutput = matrixOutput[order(matrixOutput[,1]), ]
    }
    if(!input$dispAllQSI)  {
      return(head(matrixOutput))
    } else {
      return(matrixOutput)
    }
  })
  
  generateFunctionsQSI = eventReactive(input$solveButtonQSI, {
  
    if (is.null(fileToMatrixQSI())) return(NULL)
    matrixHandler = fileToMatrixQSI()
    resultQSI = QSI(matrixHandler[,1], matrixHandler[,2])
    show("xInputQSI")
    show("solveXQSI")
    return(resultQSI$functionSet)
    
  })
  
  output$generatedFunctions = renderTable({
    toReturn = generateFunctionsQSI()
    vectorNames = c("Interval", "Function", "Range")
    colnames(toReturn) = vectorNames
    return(toReturn)
  })
  
  output$answerGivenXQSI = eventReactive(input$solveXQSI, {
    if (is.null(fileToMatrixQSI())) return(NULL)
    show("ansLabelQSI")
    round(QSISolver(fileToMatrixQSI()[,1], fileToMatrixQSI()[,2], input$xInputQSI), digits=4)
  })
  
  initialSimplexInput = reactive({
    plantsInput = c("Denver", "Phoenix", "Dallas", "Demands by")
    supplyInput = c(310, 260, 280, NA)
    w1 = c(10, 6, 3, 180)
    w2 = c(8, 5, 4, 80)
    w3 = c(6, 4, 5, 200)
    w4 = c(5, 3, 5, 160)
    w5 = c(4, 6, 9, 220)
    dfInput = data.frame(Plants=plantsInput, Supply=supplyInput, Sacramento=w1, SaltLake=w2, Chicago=w3, Albuquerque=w4, NewYorkCity=w5)
    return(dfInput)
  })
  
  solveSimplex = reactive({
    
    # Get table from ui
    tableFromUI = hot_to_r(input$inTable)

    # Extract RHS
    rhs = c()
    for(i in 3:7)
      rhs = c(rhs, -tableFromUI[4,i])
    rhs = c(rhs, tableFromUI[1,2], tableFromUI[2,2], tableFromUI[3,2], 0)
    
    # Extract the last row
    lastRow = c()
    for(i in 1:3) {
      for(j in 3:7) {
        lastRow = c(lastRow, tableFromUI[i,j])
      }
    }
    # Populate slack variables
    for(i in 1:8) {
      lastRow = c(lastRow, 0)
    }

    # Put z at the end
    lastRow = c(lastRow, 1)
    return(SimplexMin(rhs, lastRow))
    
  })
  
  generateSimplexOutput = reactive({
    
    localInputTable = hot_to_r(input$inTable)
    croppedInputTable = localInputTable[-4, -(1:2)]
    # print(croppedInputTable)
    
    resultSimplex = solveSimplex()
    finalMatrix = resultSimplex$finalMatrix
    solutionVector = resultSimplex$solutionVector
    totalEachPlant = resultSimplex$totalEachPlant
    totalEachState = resultSimplex$totalEachState
    shippingTotal = -finalMatrix[length(finalMatrix[,1]), length(finalMatrix[1,])]
    stateList = resultSimplex$stateList
    stateCount = resultSimplex$stateCount

    # Place values in their positions
    plantOutput = c("Denver", "Phoenix", "Dallas", NA, "Totals", "Shipping")
    totalOutput = c(totalEachPlant, NA, NA, shippingTotal)
    
    # Copy all number of ships
    w1 = c(solutionVector[1], solutionVector[6], solutionVector[11])
    w1 = c(w1, NA, totalEachState[1], sum(w1 * croppedInputTable[,1]))
    
    w2 = c(solutionVector[2], solutionVector[7], solutionVector[12])
    w2 = c(w2, NA, totalEachState[2], sum(w2 * croppedInputTable[,2]))
    
    w3 = c(solutionVector[3], solutionVector[8], solutionVector[13])
    w3 = c(w3, NA, totalEachState[3], sum(w3 * croppedInputTable[,3]))
    
    w4 = c(solutionVector[4], solutionVector[9], solutionVector[14])
    w4 = c(w4, NA, totalEachState[4], sum(w4 * croppedInputTable[,4]))
    
    w5 = c(solutionVector[5], solutionVector[10], solutionVector[15])
    w5 = c(w5, NA, totalEachState[5], sum(w5 * croppedInputTable[,5]))

    
    dfOutput = data.frame(Plants=plantOutput, Total=totalOutput, Sacramento=w1, SaltLake=w2, Chicago=w3, Albuquerque=w4, NewYorkCity=w5)
    return(list(dfOutput = dfOutput, stateList=stateList, stateCount=stateCount))
  })
  
  observeEvent(input$hideInput, {
    toggle("inTable")
  })
  
  output$inTable = renderRHandsontable({
    rhandsontable(initialSimplexInput()) %>%
      hot_col("Plants", readOnly=TRUE) %>%
      hot_cell(4, "Supply", readOnly=TRUE)
  })
  
  observeEvent(input$hideOutput, {
    toggle("outTable")
  })
  
  showOutputTableau = eventReactive(input$solveButtonSimplex, {
    show("oTableLabel1")
    show("oTableLabel2")
    show("showSteps")
    show("hideOutput")
    return(rhandsontable(generateSimplexOutput()$dfOutput, readOnly=TRUE))
  })
    
  output$outTable = renderRHandsontable({
    showOutputTableau()
  })
  
  observeEvent(input$showSteps, {
    show("prevStep")
    show("nextStep")
    show("hideSteps")
    show("matrixSteps")
    simplexResult = generateSimplexOutput()
    stateListGlobal = simplexResult$stateList
    maxPageNo(simplexResult$stateCount)
    pageNo(1)
  })
  
  observeEvent(input$hideSteps, {
    toggle("matrixSteps")
  })
  
  getMatrixSteps = reactive({
    simplexOutput = generateSimplexOutput()
    return(simplexOutput$stateList[[pageNo()]])
  })
  
  output$matrixSteps = renderPrint({
    getMatrixSteps()
  })
  
  observeEvent(input$nextStep, {
    if(pageNo() == maxPageNo()) return(NULL)
    pageNo(pageNo()+1)
    
  })
  
  observeEvent(input$prevStep, {
    if(pageNo() == 1) return(NULL)
    pageNo(pageNo()-1)
  })
  
}


