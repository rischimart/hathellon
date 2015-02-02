def foo_explode():
    try:
        f = open("nonexistent", 'r')
     except:
        f.close()

foo_explode()
        
