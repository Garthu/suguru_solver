
region::Int -> [[Int]]
region 5 = [
    [0,0,1,1,1],
	[0,0,0,2,1],
	[3,3,2,2,2],
	[4,3,3,2,5],
	[4,4,3,5,5]]

input::Int -> [[Int]]
input 5 = [
    [5,0,0,0,0],
	[0,0,0,0,0],
	[0,2,3,2,0],
	[0,0,0,0,0],
	[0,1,0,0,1]]

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
                                 | (x > matrixSize matrix) = False
                                 | (y > matrixSize matrix) = False
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



solverSuguru::[[Int]] -> [[Int]] -> [Int] -> Int -> Int -> Int-> [[Int]]
solverSuguru inputMatrix regionMatrix regionVector x y currentValue = do
                                if (compareElements inputMatrix 0 x y) then
                                    setMatrixValue inputMatrix currentValue x y 0
                                    #TODO
                                

solver::[[Int]] -> [[Int]]
solver matrix = do
                let regionMatrix = region (matrixSize matrix)
                let regionVector = getVectorRegions matrix
                solverSuguru matrix regionMatrix regionVector 0 0 1

main = do
    print (solver (input 5))