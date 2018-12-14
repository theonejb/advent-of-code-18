from collections import deque

def new_game():
  return {
    "board": [3, 7],
    "elf1": 0,
    "elf2": 1,
  }

def print_game(game):
  for i, r in enumerate(game["board"]):
    elf1 = game["elf1"]
    elf2 = game["elf2"]

    if i == elf1:
      print(f'({r})', end=' ')
    elif i == elf2:
      print(f'[{r}]', end=' ')
    else:
      print(r, end=' ')
  print('')

def get_new_recipes(game):
  elf1_r = game["board"][game["elf1"]]
  elf2_r = game["board"][game["elf2"]]
  new_recipe = elf1_r + elf2_r
  return map(int, list(str(new_recipe)))

def advance_game(game):
  new_recipes = get_new_recipes(game)
  elf1_r = game["board"][game["elf1"]]
  elf2_r = game["board"][game["elf2"]]

  game["board"].extend(new_recipes)

  board_len = len(game["board"])
  elf1_new = (game["elf1"] + elf1_r + 1) % board_len
  elf2_new = (game["elf2"] + elf2_r + 1) % board_len

  game["elf1"] = elf1_new
  game["elf2"] = elf2_new

def advance_game_until_board_length(game, max_length, do_print=False):
  while len(game["board"]) < max_length:
    advance_game(game)

    if do_print:
      print_game(game)

def advance_game_until_find_substring(game, sub_string, do_print=False):
  num_iterations = 0
  while True:
    advance_game(game)
    num_iterations += 1
    if num_iterations % 1000 == 0:
      print(num_iterations)

    if do_print:
      print_game(game)

    ss_l = len(sub_string)
    for i in range(ss_l - 1, -1, -1):
      reverse_index = i - ss_l
      if int(sub_string[reverse_index]) != game["board"][reverse_index]:
        break
    else:
      print("Found. Num recipes before substring: ", len(game["board"]) - ss_l)
      break

    for i in range(ss_l - 1, -1, -1):
      reverse_index = i - ss_l
      if int(sub_string[reverse_index]) != game["board"][reverse_index - 1]:
        break
    else:
      print("Found. Num recipes before substring: ", len(game["board"]) - ss_l - 1)
      break

def ten_after_n(n):
  game = new_game()
  advance_game_until_board_length(game, n + 10)
  return "".join(map(str, game["board"][n:n+10]))
