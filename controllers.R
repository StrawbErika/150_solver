solveEquation = function(originalMatrix, matrixToSolve, rowCount) {
  
  solutionSet = c()
  rightHandSide = length(matrixToSolve[1, ])
  
  # Initialize solution set, starting from the last variable
  solutionSet[rowCount] = matrixToSolve[rowCount, rightHandSide] / matrixToSolve[rowCount, rightHandSide - 1]
  
  # Formula from handout
  for(i in seq(rowCount - 1, 1, -1))
    solutionSet[i] = (matrixToSolve[i, rightHandSide] - sum(matrixToSolve[i, (i+1):rowCount] * solutionSet[(i+1):rowCount] )) / matrixToSolve[i, i]
  
  return(solutionSet)
}

translateToMatrix = function(vector1, vector2) {
  
  # Translates the two vectors into matrix
  toBeMatrix = c(vector1, vector2)
  toReturn = matrix(toBeMatrix, ncol = 2, nrow = length(vector1), dimnames = list(c(1:length(vector1)), c("x", "y")))
  return(toReturn)
}

sortRowMatrix = function(matrixToSort, i, rowCount) {
  
  # Copy the passed matrix, initialize a row holder, and set a boolean if pivoting should be done
  matrixToReturn = matrixToSort
  pivoted = FALSE
  rowTemp = 0
  
  # Initialize the max element/diagonal element, get the row it belongs to, and the index of that row
  maxElement = abs(matrixToReturn[i, i])
  maxElementRow = matrixToReturn[i, ]
  maxElementIndex = i
  
  
  for (j in seq(i+1, rowCount)) {
    
    # If the diagonal element of current row is greater than the existing one, update.
    if( abs(matrixToSort[j, i]) > maxElement ) {
      maxElement = abs(matrixToSort[j, i])
      maxElementRow = matrixToReturn[j, ]
      maxElementIndex = j
      pivoted = TRUE
    }
  }
  
  # If pivoted was set to true, swap rows.
  if (pivoted) {
    # cat("Pivoting......\n")
    rowTemp = matrixToReturn[i, ]
    matrixToReturn[i, ] = maxElementRow
    matrixToReturn[maxElementIndex, ] = rowTemp
  }
  
  return(matrixToReturn)
}

GaussJordanElimination = function(matrixToReceive) {
  
  # Makes the matrix form
  toReturn = list()
  currentLinearSystem = matrixToReceive$matrix
  varNames = matrixToReceive$variables
  
  # Initialize essential variables
  rowCount = length(currentLinearSystem[, 1])
  multiplier = 0
  solutionSet = c()
  
  # Based on handout
  for(i in seq(1, rowCount)) {
    
    if (i != rowCount){
      currentLinearSystem = sortRowMatrix(currentLinearSystem, i, rowCount)
      if( currentLinearSystem[i, i] == 0) {
        print("UNSOLVABLE.")
        return(NA)
      }
    }
    
    # print(currentLinearSystem)
    currentLinearSystem[i, ] = currentLinearSystem[i, ]  / currentLinearSystem[i, i]
    
    for (j in seq(1, rowCount)) {
      
      if (i == j) 
        next
      
      normalizedRow = currentLinearSystem[i, ] * currentLinearSystem[j, i]
      currentLinearSystem[j, ] = currentLinearSystem[j, ] - normalizedRow
      
    }
    
    # print(currentLinearSystem)
  }
  
  
  solutionSet = solveEquation(linearSystemLocal, currentLinearSystem, rowCount)
  names(solutionSet) = varNames
  toReturn = list(solutionSet = solutionSet, variables = varNames, matrix = currentLinearSystem)
  # print(toReturn)
  return(toReturn)
  
}

NaiveGJE = function(matrixToReceive) {
  
  # Makes the matrix form
  toReturn = list()
  currentLinearSystem = matrixToReceive
  
  # Initialize essential variables
  rowCount = length(currentLinearSystem[, 1])
  multiplier = 0
  solutionSet = c()
  
  # Based on handout
  for(i in seq(1, rowCount)) {
    
    if (i != rowCount){
      currentLinearSystem = sortRowMatrix(currentLinearSystem, i, rowCount)
      if( currentLinearSystem[i, i] == 0) {
        print("UNSOLVABLE.")
        return(NA)
      }
    }
    
    # print(currentLinearSystem)
    currentLinearSystem[i, ] = currentLinearSystem[i, ]  / currentLinearSystem[i, i]
    
    for (j in seq(1, rowCount)) {
      
      if (i == j) 
        next
      
      normalizedRow = currentLinearSystem[i, ] * currentLinearSystem[j, i]
      currentLinearSystem[j, ] = currentLinearSystem[j, ] - normalizedRow
      
    }
    
    # print(currentLinearSystem)
  }
  
  
  solutionSet = solveEquation(linearSystemLocal, currentLinearSystem, rowCount)
  toReturn = list(solutionSet = solutionSet, matrix = currentLinearSystem)
  return(toReturn)
  
}

setLinearSystem = function(dataSet, degreeN) {
  
  # == The size of the linear system should be degreeN+1 x degreeN+2 == #
  
  # Append to toBeMatrix the results of each computation
  toBeMatrix = c()
  nameVector = c()
  
  
  # Implements the algorithm given
  for (i in seq(1, degreeN + 1)) {
    
    iterator = 0
    exponent = i - 1
    
    while (iterator <= degreeN) {
      toBeMatrix = c(toBeMatrix, sum(dataSet[, 1] ^ exponent))
      iterator = iterator + 1
      exponent = exponent + 1
    }
    toBeMatrix = c(toBeMatrix, sum((dataSet[, 1] ^ (i - 1)) * (dataSet[, 2])))
  }
  
  # Setup names on columns
  nameVector = c()
  for (i in 1:(degreeN + 1))
     nameVector = c(nameVector, paste("a", i-1, sep=""))
  nameVector = c(nameVector, "RHS")
  
  # Setup the matrix
  toBeMatrix = matrix(toBeMatrix, ncol = degreeN + 2, nrow = degreeN + 1, dimnames = list(c(1:(degreeN+1)), nameVector), byrow = TRUE)
  toReturn = list(variables = nameVector[1:length(nameVector) - 1], matrix = toBeMatrix)
  # print(toReturn)
  return(toReturn)
  
}

PolynomialRegression = function(independentVector, dependentVector, degreeN) {
 
  # Check if vectors are same length
  if (length(independentVector) != length(dependentVector))
    return(NA)
  
  # Check if degree is within the valid range
  if (degreeN >= length(independentVector))
    return(NA)
  
  # Matrix form of the two vectors
  matrixForm = translateToMatrix(independentVector, dependentVector)
  # print(matrixForm)
  systemOfEquation = setLinearSystem(matrixForm, degreeN)
  # print(systemOfEquation)
  
  # The output after Gauss methods
  gaussOutput = GaussJordanElimination(systemOfEquation)
  coefficients = gaussOutput$solutionSet
  # print(gaussOutput)
  
  # Setup the function form
  textForm = "function (x) "
  for(i in seq(length(coefficients), 1, -1)) {
    if (i == 1) 
      textForm = paste(textForm, coefficients[i], sep = "")
    else if (i == 2) 
      textForm = paste(textForm, coefficients[i], " * x", " + ", sep = "")
    else
      textForm = paste(textForm, coefficients[i], " * x^", i - 1, " + ", sep = "")
  }
  
  functionForm = eval(parse(text = textForm))
  toReturn = list(coefficients = coefficients, functionForm = functionForm, textForm = textForm)
  # print(toReturn)
  return(toReturn)
  
}

PRSolver = function(independentVector, dependentVector, degreeN, x) {
  
  solutionSet = PolynomialRegression(independentVector, dependentVector, degreeN)
  return(solutionSet$functionForm(x))
  
}

QSI = function(independentVector, dependentVector) {
  
  # Check if vectors are same length
  if (length(independentVector) != length(dependentVector))
    return(NA)

  intervals = length(independentVector) - 1
  unknowns = intervals * 3
  rangeOfValues = independentVector
  matrixForm = translateToMatrix(independentVector, dependentVector)
  matrixOfEquations = matrix(0, nrow=unknowns, ncol=unknowns)
  solutionSet = NULL
  functionSet = matrix(0, nrow=intervals, ncol=3)
  
  # print(matrixForm)
  # cat("\n")

  firstEnd = ((length(matrixForm[,1]) - 2) * 2 + 2)
  secondEnd = firstEnd + 1
  
  
  # Generate first set of equations
  currentCol = 1
  rowAccessor = 1
  for (i in seq(1, firstEnd, by=2)) {

    matrixOfEquations[i, currentCol] = matrixForm[rowAccessor, 1]^2
    matrixOfEquations[i, currentCol+1] = matrixForm[rowAccessor, 1]
    matrixOfEquations[i, currentCol+2] = 1
    
    matrixOfEquations[i+1, currentCol] = matrixForm[rowAccessor+1, 1]^2
    matrixOfEquations[i+1, currentCol+1] = matrixForm[rowAccessor+1, 1]
    matrixOfEquations[i+1, currentCol+2] = 1
    
    currentCol = currentCol + 3
    rowAccessor = rowAccessor + 1
  }
  
  
  # Generate second set of equations in middle
  currentCol = 1
  rowAccessor = 2
  for(i in (secondEnd:(unknowns-1))) {
    matrixOfEquations[i, currentCol] = 2 * matrixForm[rowAccessor, 1] 
    matrixOfEquations[i, currentCol+1] = 1 
    matrixOfEquations[i, currentCol+3] = -2 * matrixForm[rowAccessor, 1] 
    matrixOfEquations[i, currentCol+4] = -1
    rowAccessor = rowAccessor + 1
    currentCol = currentCol + 3
  }
  
  # Last special case
  matrixOfEquations[unknowns, 1] = 1

  # Add right hand side
  rhs = c()
  rowAccessor = 1
  for (i in seq(1, firstEnd, by=2)) {
    rhs = c(rhs, matrixForm[rowAccessor, 2], matrixForm[rowAccessor+1,2])
    rowAccessor = rowAccessor + 1
  }
  for(i in (secondEnd:unknowns)) {
   rhs = c(rhs, 0) 
  }
  matrixOfEquations = cbind(matrixOfEquations, rhs)
  gjResult = NaiveGJE(matrixOfEquations)
  solutionSet = matrix(gjResult$solutionSet, nrow=intervals, ncol=3, byrow=TRUE)
  
  # print(solutionSet)
  
  
  # Generate functions
  for(i in (1:intervals)) {
    textForm = paste("function (x) ", round(solutionSet[i, 1], digits=4), " * x^2 + ", round(solutionSet[i,2], digits=4), " * x + ", round(solutionSet[i,3], digits=4), sep="")
    rangeForm = paste(matrixForm[i,1], "   <= x <= ", matrixForm[i+1,1], sep="")    
    functionSet[i, 1] = i
    functionSet[i, 2] = textForm
    functionSet[i, 3] = rangeForm
    textForm = ""
    rangeForm = ""
  }
  
  # print(functionSet)
  return(list(matrixForm = matrixForm, functionSet = functionSet))
  
}

QSISolver = function(independentVector, dependentVector, x) {
  
  solutionSet = QSI(independentVector, dependentVector)
  functionSet = solutionSet$functionSet[,2]
  rangeSet = c(solutionSet$matrixForm[,1])
  
  # Search for the appropriate function
  for(i in 1:length(rangeSet)) {
    if(x >= rangeSet[i] && x < rangeSet[i+1]) {
      corFunction = eval(parse(text = functionSet[i]))
      break
    }
  }
  return(corFunction(x))
}

initializeMatrix = function() {
  toRet = c(-1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
            0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
            0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
            0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0)
  
  matForm = matrix(toRet, nrow=8, ncol=24, byrow=TRUE)
  return(matForm)
}

SimplexMin = function(rhs, lastRow) {
  
  masterM = initializeMatrix();

  retColNames = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "Z", "RHS")
  retRowNames = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "Z")
  
  masterM = rbind(masterM, lastRow)
  masterM = cbind(masterM, rhs)
  rownames(masterM) = retRowNames
  colnames(masterM) = retColNames

  # print(masterM)

  masterLocalCopy = masterM
  # print(masterLocalCopy)
  masterMRows = length(masterM[,1])
  masterMCols = length(masterM[1,])
  
  iterationList = list()
  stateCounter = 1
  
  iterationList[[stateCounter]] = masterM
  stateCounter = stateCounter + 1
  
  # Phase 1 - until no negative in RHS, except the last row
  while(sum(masterM[-masterMRows,][,masterMCols] < 0) > 0) {
    
    # Find the least value in RHS, excluding the last row
    pivotRowIndex = which.min(masterM[-masterMRows,][,masterMCols])
    maxNegInRHS = masterM[pivotRowIndex, length(masterM[1,])]
    # cat("Pivot row index:", pivotRowIndex, "\nPivot row element:", maxNegInRHS, "\n")
    
    # Find the least value in leastValRHS row
    rowToDivide = masterM[, -length(masterM[1,])]
    rowToDivide = rowToDivide[pivotRowIndex, ]
    pivotColumnIndex = which.max(rowToDivide/maxNegInRHS)
    pivotElement = masterM[pivotRowIndex, pivotColumnIndex]
    # cat("Pivot element @", pivotRowIndex, pivotColumnIndex, ":", pivotElement, "\n")
    
    # Normalize the row
    masterM[pivotRowIndex, ] = masterM[pivotRowIndex, ]/pivotElement
    # pivotElement = masterM[pivotRowIndex, pivotColumnIndex]
    # cat("New pivot element: ", pivotElement, "\n")
  
    iterationList[[stateCounter]] = masterM
    stateCounter = stateCounter + 1
    
    for(i in 1:length(masterM[,1])) {
      if(i == pivotRowIndex)
        next
      
      normalizedRow = masterM[pivotRowIndex, ] * masterM[i, pivotColumnIndex]
      masterM[i, ] = masterM[i, ] - normalizedRow
      
      
    }
  }
  
  
  # Phase 2 - while there is a negative on the last row, except on last column.
  while(sum(masterM[,-masterMCols][masterMRows,] < 0) > 0 ) {


    # Find the least value in the last row, except on RHS
    pivotColIndex = which.min(masterM[,-masterMCols][masterMRows,])
    maxNegInLast = masterM[masterMRows, pivotColIndex]

    # Find row index of pivot element
    pivotRowIndex = which.max(masterM[,pivotColIndex]/masterM[,masterMCols])
    pivotElement = masterM[pivotRowIndex, pivotColIndex]
    # cat("Row:", pivotRowIndex, "Col:", pivotColIndex, "\n")

    # Normalize
    masterM[pivotRowIndex, ] = masterM[pivotRowIndex, ]/pivotElement
    
    iterationList[[stateCounter]] = masterM
    stateCounter = stateCounter + 1
    
    for(i in 1:masterMRows) {
      if(i == pivotRowIndex)
        next
      normalizedRow = masterM[pivotRowIndex, ] * masterM[i, pivotColIndex]
      masterM[i, ] = masterM[i, ] - normalizedRow

    }
  }

  # Final state
  iterationList[[stateCounter]] = masterM

  # Extract values from each column
  solutionVector = c()
  
  # Iterate through all columns minus slack variables
  for(i in 1: (masterMCols - masterMRows - 1)) {
    
    # Add to solution vector when there is only one 1 in column.
    if(sum(masterM[,i] == 1) == 1) {
      solutionRow = match(c(1), masterM[,i])
      solutionVector = c(solutionVector, masterM[solutionRow, masterMCols])
    } else {
      solutionVector = c(solutionVector, 0)
    }
  }
  
  
  totalEachPlant = c()
  totalEachState = c()
  
  # Each plant
  holder = 0
  for(i in 1:5) {
    holder = holder + solutionVector[i]
  }
  totalEachPlant = c(totalEachPlant, holder)
  
  holder = 0
  for(i in 6:10) {
    holder = holder + solutionVector[i]
  }
  totalEachPlant = c(totalEachPlant, holder)
  
  holder = 0
  for(i in 11:15) {
    holder = holder + solutionVector[i]
  }
  totalEachPlant = c(totalEachPlant, holder)  
  
  # Each state
  for(i in 1:5) {
    count = 1
    holder = 0
    while(count <= 3) {
      holder = holder + solutionVector[i]
      i = i+5
      count = count+1
    }
    totalEachState = c(totalEachState, holder)
  }
  
  return(list(finalMatrix = masterM, solutionVector = solutionVector, totalEachPlant = totalEachPlant, totalEachState = totalEachState, stateList=iterationList, stateCounter=stateCounter))
  
}






# independentVector = c(50, 50, 50, 70, 70, 70, 80, 80, 80, 90, 90, 90, 100, 100, 100)
# dependentVector = c(3.3, 2.8, 2.9, 2.3, 2.6, 2.1, 2.5, 2.9, 2.4, 3.0, 3.1, 2.8, 3.3, 3.5, 3.0)
# result = PRSolver(independentVector, dependentVector, 5, 5)
# print(result)

# independentVector = c(0, 10, 15, 20, 22.5, 30)
# dependentVector = c(0, 227.04, 362.78, 517.35, 602.97, 901.67)
# independentVector = c(3.0, 4.5, 7.0, 9.0)
# dependentVector = c(2.5, 1.0, 2.5, 0.5)
# result = QSISolver(independentVector, dependentVector, 5)
# result = QSI(independentVector, dependentVector)
# print(result)

# initializeMatrix()
