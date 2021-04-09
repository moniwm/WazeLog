
def draw(code_list):
    for i in range(0, len(code_list)):
        for j in range(0, len(code_list[i])):
            if j % 2 == 0:
                draw_blank(code_list[i][j])
            else:
                draw_star(code_list[i][j])
        print(" ")

def draw_star(num):
    for i in range(0, num):
        print("*", end ="")

def draw_blank(num):
    for i in range(0, num):
        print(" ", end="")

print("\nDibujo de Gus")
draw([[3,3],[0,1,1,5,1,1],[0,3,1,1,1,3],[2,5],[3,3],[3,1,1,1],[2,2,1,2]])
print("\n\n")
print("Dibujo de Isa")
draw([[1,3,3,3,1],[0,1,3,1,1,1,3,1],[0,1,4,1,4,1],[0,1,9,1],[0,1,9,1],[0,1,9,1],[1,1,7,1,1],[2,1,5,1,2],[3,1,3,1,3],[4,1,1,1,4],[5,1,5]])
