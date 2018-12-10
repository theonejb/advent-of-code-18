from collections import deque, defaultdict
from itertools import cycle

def print_board(board):
    print(' '.join(str(marble) for marble in board))

def move(board, marble):
    if marble % 23 == 0:
        board.rotate(7)
        removed_marble = board.pop()
        board.rotate(-1)
        return marble + removed_marble
    else:
        board.rotate(-1)  # Because we rotate to the back to move head clock wise to where we need to insert
        board.append(marble)
        return 0

def play_game(num_players, num_marbles):
    board = deque([0])
    scores = defaultdict(lambda: 0)

    players = cycle(range(1, num_players + 1))

    for m in range(1, num_marbles + 1):
        # print_board(board)
        current_player = next(players)
        score = move(board, m)
        scores[current_player] += score

    # print_board(board)
    sorted_scores = sorted(scores.values(), reverse=True)
    print(f'Highest score {sorted_scores[0]}')

def time_and_play_game(num_players, num_marbles):
    import time
    start_time = time.perf_counter()
    play_game(num_players, num_marbles)
    end_time = time.perf_counter()

    time_taken = end_time - start_time
    print("Took {:0.2f}ms to play game".format(time_taken * 1000))