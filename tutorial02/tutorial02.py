def dbzDialog(powerLevel):
  if powerLevel > 9000:
    return 100
  else:
    return "Meh, not impressive..."

def switchNum(n):
  match n:
    case < 0:
      return -1
    case n == 0:
      return 0
    case _:
      return 1

print(dbzDialog(90000))