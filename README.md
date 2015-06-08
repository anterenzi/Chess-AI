# Chess-AI
Simple Chess AI. Coursework. Common Lisp.

# Methodology
The chess algorithmn uses a MiniMax approach with Alpha-beta pruning to compute the next best move.
The larger the depth, the longer the algorithmn takes but the more moves it reads ahead.
Whenever it reaches the bottom depth, expanding the tree as it goes along,
it uses a static evaluation function to determine how advantageous the board state is to each player.
