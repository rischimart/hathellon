for c in [1, 2, 4, 8]:
  for a, b in [(1, 2), (3, 4), (5, 6), (7, 8)]:
   if a > 3:
    break
   print a + c
   print b * "*"
