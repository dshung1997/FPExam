import random
a = []
for i in range(66):
    x = random.randint(1, 5)
    a.append(x)

print("val listRandom = ", end="")
print(a)


b = [34, 32, 28, 28, 6, 16, 32, 40, 34, 26, 16, 56, 48, 48, 50, 18, 32, 12, 12, 38, 28, 30, 18, 30, 26, 42, 30, 40, 38, 52, 42, 14, 24, 40, 32, 52, 32, 44, 20, 18, 22, 30, 20, 8, 28, 58, 36, 32, 30, 36, 22, 22, 24, 40, 40, 42, 36, ~6, 52, 38, 48, 26, 50, 42, 28, 48, 32, 38, 14, 22, 24, 24, 30, 60, 34, 42, 32, 48, 44, 18, 46, 38, 44, 38, 44, 18, 18, 32, 48, 16, 38, 40, 36, 44, 60, 24, 32, 32, 42, 28]

l = []
sum1 = 0
for i in range(20):
    sum1 += b[i]
# print(sum1)
l.append(sum1)

for i in range(len(b)-20):
    sum2 = sum1 + b[20+i] - b[i]
    # print(sum2)
    l.append(sum2)

print()
print(sum(l) / len(l))
print(min(l))
print(max(l))

b.sort()
b1 = [b[i] for i in range(20)]
print(sum(b1))