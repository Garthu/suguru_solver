-- Suguru Solver using Haskell
type Matriz = [[Int]]
type Coordenada = (Int,Int)


region:: Int -> [[Int]]
region 5 = [
    [0,0,1,1,1],
	[0,0,0,2,1],
	[3,3,2,2,2],
	[4,3,3,2,5],
	[4,4,3,5,5]]

input::Int -> [[Int]]
input 5 = [
    [5,1,1,1,1],
	[0,0,0,0,0],
	[0,2,3,2,0],
	[0,0,0,0,0],
	[0,1,0,0,1]]

tabuleiro5x5 = [[(0,0),(0,1),(1,0),(1,1),(1,2)],
			[(0,2),(0,3),(0,4),(1,4)],
			[(1,3),(2,2),(2,3),(2,4),(3,3)],
			[(3,0),(4,0),(4,1)],
			[(2,0),(2,1),(3,1),(3,2),(4,2)],
			[(3,4),(4,3),(4,4)]]


tabuleiro6x6  = [[(0,0),(0,1),(1,0),(2,0)],
			[(0,2),(1,1),(1,2),(1,3),(2,2)],
			[(0,3),(0,4),(0,5),(1,4),(1,5)],
			[(2,1),(3,0),(3,1),(3,2),(4,1)],
			[(2,3),(2,4),(2,5),(3,3),(3,4)],
			[(4,2),(4,3)],
			[(4,0),(5,0),(5,1),(5,2),(5,3)],
			[(3,5),(4,4),(4,5),(5,4),(5,5)]]

tabuleiro7x7  = [[(0,0),(1,0)],
			[(0,1),(1,1),(1,2),(2,1)],
			[(0,2),(0,3),(0,4),(0,5),(1,3)],
			[(0,6),(1,5),(1,6),(2,6),(3,6)],
			[(2,0),(3,0),(3,1),(4,0),(4,1)],
			[(2,2),(3,2),(3,3),(4,2)],
			[(1,4),(2,3),(2,4),(2,5),(3,4)],
			[(5,0),(5,1),(6,0),(6,1),(6,2)],
			[(4,3),(5,2),(5,3),(5,4),(6,3)],
			[(3,5),(4,4),(4,5),(4,6),(5,5)],
			[(5,6),(6,4),(6,5),(6,6)]]

--Procura na matriz uma coordenada valida para inserir 

elemento :: [[Int]] -> Coordenada -> Int
elemento mat (x,y) = mat !! x !! y




find_valid :: [[Int]] -> Coordenada -> Coordenada
find_valid mat (x,y) | elemento mat(x,y) == 0 = (x,y)
			       | x == length mat && y == length mat = (-1,-1)
			       | y == length mat && x /= length mat = find_valid mat (x+1,0)
			       | otherwise = find_valid mat (x,y+1)



--Verifica se é valido o chute

--is_valid :: Matriz -> Int -> Coordenada -> Bool
--is_valid M kick (x,y) | length M == 5 = metodo5x5
--						where metodo5x5 = 
					

-- Resolve o Suguro, retornando o problema resolvido

--solver_suguru :: Matriz -> Matriz
--solver_suguru (a:b) 



main = do

    --matrix = [[5, 0, 0, 0, 0],[0, 0, 0, 0, 0],[0, 2, 3, 2, 0],[0, 0, 0, 0, 0],[0, 1, 0, 0, 1]]
    
	print(find_valid (input 5) (0,0))
    
