arr=[input()for i in range(4)]
K=int(input())
result = 0

stack=[]
def rotationDown(arr, d, i):
    global direct
    if d == -1:
        if arr[i][2] != arr[i + 1][6]:
            direct = 1
            stack.append((i+1,direct))
            return
    if d == 1:
        if arr[i][2] != arr[i + 1][6]:
            direct = -1
            stack.append((i+1, direct))
            return
    direct = 0

def rotationUp(arr, d, i):
    global direct
    if d == -1:
        if arr[i][6] != arr[i - 1][2]:
            direct = 1
            stack.append((i-1, direct))
            return
    if d == 1:
        if arr[i][6] != arr[i - 1][2]:
            direct = -1
            stack.append((i-1, direct))
            return
    direct = 0

def move(arr,stack):
    for _ in range(len(stack)):
        i,d=stack.pop()
        if d==1:
            arr[i]=arr[i][-1]+arr[i][:-1]
        if d==-1:
            arr[i]=arr[i][1:]+arr[i][0]

# print(arr)
for o in range(K):
    N, D = map(int, input().split())
    N = N - 1
    direct = D

    stack.append((N, direct))
    if N == 0:
        for i in range(0, 3):
            ### 아래로만(톱니 우측만)
            if direct == 0:
                break
            if direct == 1:
                rotationDown(arr, 1, i)
                continue
            if direct == -1:
                rotationDown(arr, -1, i)
                continue
    if N == 3:
        for i in range(3, 0, -1):
            ### 위로만(톱니 좌측만)
            if direct == 0:
                break
            if direct == 1:
                rotationUp(arr, 1, i)
                continue
            if direct == -1:
                rotationUp(arr, -1, i)
                continue
    if N == 1 or N == 2:
        for i in range(N, 0, -1):
            ## 위로(톱니 좌측)
            if direct == 0:
                break
            if direct == 1:
                rotationUp(arr, 1, N)
                continue
            if direct == -1:
                rotationUp(arr, -1, N)
                continue
        direct = D
        for i in range(N, 3):
            ## 아래로(톱니 우측)
            if direct == 0:
                break
            if direct == 1:
                rotationDown(arr, 1, N)
                continue
            if direct == -1:
                rotationDown(arr, -1, N)
                continue
    # print(stack)
    move(arr,stack)
    # print(arr)

if arr[0][0] == '1':
    result += 1
if arr[1][0] == '1':
    result += 2
if arr[2][0] == '1':
    result += 4
if arr[3][0] == '1':
    result += 8
print(result)

