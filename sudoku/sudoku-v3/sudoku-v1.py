
import numpy as np

grid = [[5,3,0,0,7,0,0,0,0], 
        [6,0,0,1,9,5,0,0,0],
        [0,9,8,0,0,0,0,6,0], 
        [8,0,0,0,6,0,0,0,3], 
        [4,0,0,8,0,3,0,0,1], 
        [7,0,0,0,2,0,0,0,6], 
        [0,6,0,0,0,0,2,8,0], 
        [0,0,0,4,1,9,0,0,5], 
        [0,0,0,0,8,0,0,7,9]]

#soulution
#[[5 3 4 6 7 8 9 1 2]
# [6 7 2 1 9 5 3 4 8]
# [1 9 8 3 4 2 5 6 7]
# [8 5 9 7 6 1 4 2 3]
# [4 2 6 8 5 3 7 9 1]
# [7 1 3 9 2 4 8 5 6]
# [9 6 1 5 3 7 2 8 4]
# [2 8 7 4 1 9 6 3 5]
# [3 4 5 2 8 6 1 7 9]]

def possible(y,x,n):
    global grid
    for i in range(0,9):
        if grid[y][i] == n:
            return False
        if grid[i][x] == n:
            return False

    y0 = (y//3)*3
    x0 = (x//3)*3

    for i in range(0,3):
        for j in range(0,3):
            if grid[y0+i][x0+j]== n:
                return False
    return True

def solve():
    global grid
    for y in range(9):
        for x in range(9):
            if grid[y][x] == 0:
                for n in range(1, 10):
                    if possible(y,x,n):
                        grid[y][x] = n
                        solve()
                        grid[y][x] = 0
                return 
    print(np.matrix(grid))
    input("More?") 
solve()




