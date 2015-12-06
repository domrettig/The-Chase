opam install lambda-term
cs3110 compile gui.ml -t -p yojson,str,lambda-term
cs3110 run gui.ml

Lambda-term needs to be installed prior to compiling and running. Instructions for the game are below:

Navigation of the game:
Pressing Esc at any point in the game will exit the game.
Enter is the primary method of moving through the menus

Instructions

1. ) In the first, the player attempts to answer as many questions correctly in a minute, with
each correct answer adding money to their wallet.

2. ) After this round, the player attempts to move their money from their wallet to the bank
by correctly answering more questions while avoiding being caught by “The Chaser”.
Here is how it works:
The Chaser presents the players with a few different options. They can:
start 4 spots away from the bank and wager ½ of the money in their wallet,
start 5 spots away from the bank and wager ⅔ the money in their wallet,
start 6 spots away from the bank and wager all the money in their wallet.
The Chaser will always start 8 spots away from the player. The Chaser and
the player are asked the same question, and a correct answer moves them forward. If
the Chaser catches the player, the player loses the money they wagered from their
wallet and, if any remains, move on to the final round in an attempt to double their wallet
or nothing. If the player reaches the bank before the Chaser catches them, they move to
the third round with their full wallet in addition to the amount they banked for an attempt
to either double their earnings or nothing. *Note: Player has the option to quit the game
and take home the amount in their bank or keep playing.

3. ) In the final round, the player is presented with two groups of questions, “A” and “B.”
The player selects one group and the Chaser gets the other group. The player is then
given two minutes to answer as many questions as possible correctly, with each correct
answer moving the player one spot forward. After the two minutes is up, the Chaser has
two minutes to answer questions from his group. If he gets a question wrong, the timer is
paused and the player has a chance to answer. If the player gets it right, the Chaser
moves back a spot; an incorrect answer means the Chaser doesn’t move. If the Chaser
catches up to the player, the player loses their money, but if the Chaser can’t reach the
player in two minutes, the player wins and keeps double their money