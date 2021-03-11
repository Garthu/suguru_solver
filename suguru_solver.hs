-- Suguru Solver using Haskell
type Matriz = [[Int]]
type Coordenada = (Int,Int)


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

elemento :: Matriz -> Coordenada -> Int
elemento M (x,y) = M !! x !! y

-- Nothing teria que criar um none com monad
find_valid :: Matriz -> Coordenada -> Maybe Coordenada
find_valid M (x,y) | elemento M (x,y) == 0 = Just (x,y)
					   | x == length M && y == length M = Nothing 
					   | y == length M && elemento M (x,y) == 0 = Just (x,y)
					   | y == length M = find_valid(0,y+1)
					   | otherwise = find_valid(x+1,y)



--Verifica se é valido o chute

is_valid :: Matriz -> Int -> Coordenada -> Bool
is_valid M kick (x,y) | length M == 5 = metodo5x5
						where metodo5x5 = 
					

-- Resolve o Suguro, retornando o problema resolvido

solver_suguru :: Matriz -> Matriz
solver_suguru (a:b) = 



main = do
    matrix = [[5, 0, 0, 0, 0],
			[0, 0, 0, 0, 0],
		  	[0, 2, 3, 2, 0],
		  	[0, 0, 0, 0, 0],
		  	[0, 1, 0, 0, 1]]
    
    print(matrix)
    
