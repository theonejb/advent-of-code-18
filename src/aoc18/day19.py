# My prog in Python
A = B = C = D = E = F = IP = 0

C = C + 2
C = C * C
C = 19 * C
C = C * 11
E = E + 2
E = E * 22
E = E + 2
C = C + E
if A == 1:
    E = 27
    E = E * 28
    E = 29 + E
    E = 30 * E
    E = E * 14
    E = E * 32
    C = C + E
    A = 0

B = 1
F = 1

while True:
    E = B * F
    
    if C / B == F:
        print("B: ", B, " F: ", F)
        A = B + A
    
    F = F + 1
    
    if F > C:
        B = B + 1
        F = 1
    else:
        continue

    if B > C:
        break
