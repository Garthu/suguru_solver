# Suguru solver using python
from pprint import pprint

regiao5 = [[(0,0),(0,1),(1,0),(1,1),(1,2)],
			[(0,2),(0,3),(0,4),(1,4)],
			[(1,3),(2,2),(2,3),(2,4),(3,3)],
			[(3,0),(4,0),(4,1)],
			[(2,0),(2,1),(3,1),(3,2),(4,2)],
			[(3,4),(4,3),(4,4)]]


regiao6 = [[(0,0),(0,1),(1,0),(2,0)],
			[(0,2),(1,1),(1,2),(1,3),(2,2)],
			[(0,3),(0,4),(0,5),(1,4),(1,5)],
			[(2,1),(3,0),(3,1),(3,2),(4,1)],
			[(2,3),(2,4),(2,5),(3,3),(3,4)],
			[(4,2),(4,3)],
			[(4,0),(5,0),(5,1),(5,2),(5,3)],
			[(3,5),(4,4),(4,5),(5,4),(5,5)]]

regiao7 = [[(0,0),(1,0)],
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

def find_valid(matrix):
	for rows in range(len(matrix)):
		for columns in range(len(matrix)):
			if matrix[rows][columns] == 0:
				return rows, columns

	return None, None

def is_valid(matrix, kick, row, col):
	
	if (len(matrix) == 5):
		for group in regiao5:
			if (row, col) in group:
				for tuple_value in group:
					if kick > len(group):
						return False
					if matrix[tuple_value[0]][tuple_value[1]] == kick:
						return False

	if (len(matrix) == 6):
		for group in regiao6:
			if (row, col) in group:
				for tuple_value in group:
					if kick > len(group):
						return False
					if matrix[tuple_value[0]][tuple_value[1]] == kick:
						return False

	if (len(matrix) == 7):
		for group in regiao7:
			if (row, col) in group:
				for tuple_value in group:
					if kick > len(group):
						return False
					if matrix[tuple_value[0]][tuple_value[1]] == kick:
						return False

	if row > 0:
		if matrix[row-1][col] == kick:
			return False
		if col > 0:
			if matrix[row-1][col-1] == kick:
				return False
		if col < len(matrix)-1:
			if matrix[row-1][col+1] == kick:
				return False
	if row < len(matrix)-1:
		if matrix[row+1][col] == kick:
			return False
		if col > 0:
			if matrix[row+1][col-1] == kick:
				return False
		if col < len(matrix)-1:
			if matrix[row+1][col+1] == kick:
				return False
	if col > 0:
		if matrix[row][col-1] == kick:
			return False
	if col < len(matrix)-1:
		if matrix[row][col+1] == kick:
			return False

	return True

def solver_suguru(matrix):
	row, col = find_valid(matrix)

	if row == None:
		return True

	for kick in range(1, len(matrix)+1):
		if is_valid(matrix, kick, row, col):
			matrix[row][col] = kick

			if solver_suguru(matrix):
				return True
		matrix[row][col] = 0

	return False

if __name__ == '__main__':
	matrix = [[5, 0, 0, 0, 0],
			[0, 0, 0, 0, 0],
		  	[0, 2, 3, 2, 0],
		  	[0, 0, 0, 0, 0],
		  	[0, 1, 0, 0, 1]]

	solver_suguru(matrix)
	pprint(matrix)