import Debug.Trace

region::Int -> [[Int]]
region 5 = [
    [0,1,2,2,2],
	[0,1,1,2,2],
    [3,1,4,4,4],
    [3,3,3,4,4],
    [3,5,5,5,5]]

region 6 = [
    [0,0,0,2,2,2],
	[1,1,0,0,2,2],
    [3,3,3,4,4,4],
    [7,3,6,4,4,5],
    [7,7,6,6,5,5],
    [7,7,6,6,5,5]]

input::Int -> [[Int]]
input 5 = [
    [0,0,0,0,0],
	[0,0,1,0,4],
	[0,0,0,0,0],
	[4,0,0,0,0],
	[0,0,0,0,0]]

input 6 = [
    [0,5,0,0,0,0],
	[0,0,0,0,4,5],
    [0,0,0,0,0,0],
    [5,1,0,3,0,0],
    [0,0,0,0,0,2],
    [4,0,2,0,0,0]]

-- region::Int -> [[Int]]
-- region 5 = [
--     [0,1,1,1,3],
-- 	[0,0,1,2,3],
-- 	[0,0,2,2,2],
-- 	[4,4,4,2,5],
-- 	[4,4,5,5,5]]

-- input::Int -> [[Int]]
-- input 5 = [
--     [3,0,0,0,0],
-- 	[0,0,0,4,0],
-- 	[2,0,1,0,0],
-- 	[0,0,0,0,0],
-- 	[4,0,3,1,0]]

-- region::Int -> [[Int]]
-- region 5 = [
--     [0,0,1,1,1],
-- 	[0,0,0,2,1],
-- 	[3,3,2,2,2],
-- 	[4,3,3,2,5],
-- 	[4,4,3,5,5]]

-- input::Int -> [[Int]]
-- input 5 = [
--     [5,0,0,0,0],
-- 	[0,0,0,0,0],
-- 	[0,2,3,2,0],
-- 	[0,0,0,0,0],
-- 	[0,1,0,0,1]]

setMatrixValueLine::[Int] -> Int -> Int -> Int -> [Int]
setMatrixValueLine [] _ _ _= []
setMatrixValueLine (a:b) value y at | (at == y) = value:b
                                    | otherwise = a:(setMatrixValueLine b value y (at+1))

setMatrixValue::[[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
setMatrixValue [] _ _ _ _ = []
setMatrixValue (a:b) value x y at | (x == at) = (setMatrixValueLine a value y 0):b
                                   | otherwise = a:(setMatrixValue b value x y (at + 1))

getCurrentRegionSize::[[Int]] -> [Int] -> Int -> Int -> Int
getCurrentRegionSize matrix vector x y = vector !! (getElementMatrix matrix x y)

getQntRegionsLine::[Int] -> Int -> Int
getQntRegionsLine [] max = max
getQntRegionsLine (a:b) max | (a > max) = getQntRegionsLine b a
                        | otherwise = getQntRegionsLine b max

getQntRegionsMatrix::[[Int]] -> Int -> Int
getQntRegionsMatrix [] max = max
getQntRegionsMatrix (a:b) max | (getQntRegionsLine a 0 > max) = getQntRegionsMatrix b (getQntRegionsLine a 0)
                        | otherwise = getQntRegionsMatrix b max

getQntRegions::[[Int]] -> Int
getQntRegions [] = 0
getQntRegions matrix = 1 + (getQntRegionsMatrix matrix 0)

getQntRegionValueLine::[Int] -> Int -> Int
getQntRegionValueLine [] _ = 0
getQntRegionValueLine (a:b) value | (value == a) = 1 + getQntRegionValueLine b value
                                  | otherwise = getQntRegionValueLine b value

getQntRegionValueMatrix::[[Int]] -> Int -> Int
getQntRegionValueMatrix [] _ = 0
getQntRegionValueMatrix (a:b) value = (getQntRegionValueLine a value) + (getQntRegionValueMatrix b value)

getVectorRegionsMatrix::[[Int]] -> Int -> Int -> [Int]
getVectorRegionsMatrix matrix regionValue qntRegions | (regionValue == qntRegions) = []
                                                     | otherwise = (getQntRegionValueMatrix matrix regionValue):[] ++ (getVectorRegionsMatrix matrix (regionValue+1) qntRegions)

getVectorRegions::[[Int]] -> [Int]
getVectorRegions [] = []
getVectorRegions matrix = do
                          let x = getQntRegions matrix
                          getVectorRegionsMatrix matrix 0 x


lineMatrixSize::[Int] -> Int
lineMatrixSize [] = 0
lineMatrixSize (a:b) = 1 + lineMatrixSize b

matrixSize::[[Int]] -> Int
matrixSize (a:b) = lineMatrixSize a

getElementMatrix::[[Int]] -> Int -> Int -> Int
getElementMatrix matrix x y = (matrix !! x) !! y

compareElements::[[Int]] -> Int -> Int -> Int -> Bool
compareElements matrix value x y | (x < 0) = False
                                 | (y < 0) = False
                                 | (x == matrixSize matrix) = False
                                 | (y == matrixSize matrix) = False
                                 | (value == (getElementMatrix matrix x y)) = True
                                 | otherwise = False


hasNeighbor::[[Int]] -> Int -> Int -> Int -> Bool
hasNeighbor matrix value x y = do
                               let a = compareElements matrix value x (y-1)
                               let b = compareElements matrix value x (y+1)
                               let c = compareElements matrix value (x-1) y
                               let d = compareElements matrix value (x+1) y
                               let e = compareElements matrix value (x-1) (y-1)
                               let f = compareElements matrix value (x-1) (y+1)
                               let g = compareElements matrix value (x+1) (y-1)
                               let h = compareElements matrix value (x+1) (y+1)
                               a || b || c || d || e || f || g || h


hasSameValueOnRegionLine::[Int] -> [Int] -> Int -> Int -> Bool
hasSameValueOnRegionLine [] [] _ _ = False
hasSameValueOnRegionLine (a:b) (c:d) regionValue dataValue| (c == regionValue) && (a == dataValue) = True
                        | otherwise = hasSameValueOnRegionLine b d regionValue dataValue

hasSameValueOnRegionMatrix::[[Int]] -> [[Int]] -> Int -> Int -> Bool
hasSameValueOnRegionMatrix [] [] _ _ = False
hasSameValueOnRegionMatrix (a:b) (c:d) regionValue dataValue | (hasSameValueOnRegionLine a c regionValue dataValue ) = True
                          | otherwise = hasSameValueOnRegionMatrix b d regionValue dataValue 


solverSuguruPoint::[[Int]] -> [[Int]] -> Int -> Int -> Int -> Bool
solverSuguruPoint inputMatrix regionMatrix x y currentValue | ((getElementMatrix inputMatrix  x y) == 0) = do
                                                                            if (hasNeighbor inputMatrix currentValue x y) || (hasSameValueOnRegionMatrix inputMatrix regionMatrix (getElementMatrix regionMatrix x y) currentValue) then
                                                                                False
                                                                            else
                                                                                True
                                                                        | otherwise = False

compareLine::[Int] -> [Int] -> Bool
compareLine [] [] = True
compareLine (a:b) (c:d) | (a == c) = compareLine b d
                        | otherwise = False

compareMatriz::[[Int]] -> [[Int]] -> Bool
compareMatriz [] [] = True
compareMatriz [] _ = False
compareMatriz _ [] = False
compareMatriz (a:b) (c:d) | (compareLine a c) = compareMatriz b d
                          | otherwise = False

suguruIsCompleteLine::[Int] -> Bool
suguruIsCompleteLine [] = True
suguruIsCompleteLine (a:b) | (a == 0) = False
                            | otherwise = suguruIsCompleteLine b

suguruIsComplete::[[Int]] -> Bool
suguruIsComplete [] = True
suguruIsComplete (a:b) | (suguruIsCompleteLine a) = suguruIsComplete b
                        | otherwise = False

solverSuguruItemCompareX::[[Int]] -> [[Int]] -> [[Int]] -> [Int] -> Int -> Int -> Int-> [[Int]]
solverSuguruItemCompareX inputMatrix newInputMatrix regionMatrix regionVector x y currentValue | (compareMatriz newInputMatrix (solverSuguru newInputMatrix regionMatrix regionVector 0 (y+1) 1)) = solverSuguru inputMatrix regionMatrix regionVector x y (currentValue+1) 
                                                                                            | otherwise = (solverSuguru newInputMatrix regionMatrix regionVector 0 (y+1) 1)

solverSuguruItemCompareY::[[Int]] -> [[Int]] -> [[Int]] -> [Int] -> Int -> Int -> Int-> [[Int]]
solverSuguruItemCompareY inputMatrix newInputMatrix regionMatrix regionVector x y currentValue | (compareMatriz newInputMatrix (solverSuguru newInputMatrix regionMatrix regionVector (x+1) y 1)) = solverSuguru inputMatrix regionMatrix regionVector x y (currentValue+1) 
                                                                                            | otherwise = (solverSuguru newInputMatrix regionMatrix regionVector (x+1) y 1)

solverSuguruItem::[[Int]] -> [[Int]] -> [[Int]] -> [Int] -> Int -> Int -> Int-> [[Int]]
solverSuguruItem inputMatrix newInputMatrix regionMatrix regionVector x y currentValue | ((x+1) == matrixSize inputMatrix) && ((y+1) == matrixSize inputMatrix) = newInputMatrix
                                                                                    | ((x+1) == matrixSize inputMatrix) = solverSuguruItemCompareX inputMatrix newInputMatrix regionMatrix regionVector x y currentValue
                                                                                    | otherwise = solverSuguruItemCompareY inputMatrix newInputMatrix regionMatrix regionVector x y currentValue

solverSuguruItemOtherwise::[[Int]] -> [[Int]] -> [Int] -> Int -> Int -> Int-> [[Int]]
solverSuguruItemOtherwise inputMatrix regionMatrix regionVector x y currentValue | ((x+1) == matrixSize inputMatrix) && ((y+1) == matrixSize inputMatrix) = inputMatrix
                                                                                    | ((x+1) == matrixSize inputMatrix) = solverSuguru inputMatrix regionMatrix regionVector 0 (y+1) 1
                                                                                    | otherwise = solverSuguru inputMatrix regionMatrix regionVector (x+1) y 1


solverSuguru::[[Int]] -> [[Int]] -> [Int] -> Int -> Int -> Int-> [[Int]]
solverSuguru inputMatrix regionMatrix regionVector x y currentValue | (currentValue > ((getCurrentRegionSize regionMatrix regionVector x y))) = inputMatrix
                                                                    | (solverSuguruPoint inputMatrix regionMatrix x y currentValue) = do
                                                                        let newInputMatrix = setMatrixValue inputMatrix currentValue x y 0
                                                                        solverSuguruItem inputMatrix newInputMatrix regionMatrix regionVector x y currentValue

                                                                    | ((getElementMatrix inputMatrix x y) == 0) = solverSuguru inputMatrix regionMatrix regionVector x y (currentValue+1) 
                                                                    | otherwise = solverSuguruItemOtherwise inputMatrix regionMatrix regionVector x y currentValue
                                                                        


solver::[[Int]] -> [[Int]]
solver matrix = do
                let regionMatrix = region (matrixSize matrix)
                let regionVector = getVectorRegions regionMatrix
                solverSuguru matrix regionMatrix regionVector 0 0 1

main = do
    let reg = region 5
    let inp = input 5
    let vecReg = getVectorRegions reg

    print (solver (input 6))