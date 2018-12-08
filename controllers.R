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

translateToMatrix = function(vector1, vector2) {
  
  # Translates the two vectors into matrix
  toBeMatrix = c(vector1, vector2)
  toReturn = matrix(toBeMatrix, ncol = 2, nrow = length(vector1), dimnames = list(c(1:length(vector1)), c("Xi", "Yi")))
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

independentVector = c(50, 50, 50, 70, 70, 70, 80, 80, 80, 90, 90, 90, 100, 100, 100)
dependentVector = c(3.3, 2.8, 2.9, 2.3, 2.6, 2.1, 2.5, 2.9, 2.4, 3.0, 3.1, 2.8, 3.3, 3.5, 3.0)
result = PolynomialRegression(independentVector, dependentVector, 5)
# print(result)
