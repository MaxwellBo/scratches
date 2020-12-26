for (j, k) in [(17, 6), (72, 5), (77, 24), (102, 27)]:
  x = 0

  for i in range(0, k):
      x += j ** i

  print("{" + str(x) + ", " + str(j) + "},")
