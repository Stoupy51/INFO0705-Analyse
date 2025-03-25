
""" Python script that fixes the average ping for the given csv file and removes duplicates.

This script calculates the average ping from non-NULL values and replaces NULL values with this average.
It also removes duplicate entries based on player names.
"""

# Imports
import csv


# Constants
FILE_NAME: str = "data_analysis.csv"
AVG_POS: int = 6


# Get avg ping
avg: float = 0
count: int = 0
with open(FILE_NAME, "r") as file:
	reader = csv.reader(file)
	next(reader)

	for row in reader:
		if row[AVG_POS] != "NULL":
			avg += float(row[AVG_POS])
			count += 1
avg /= count
print("Final average ping: " + str(avg))

# Fix csv file and remove duplicates
seen_players: dict = {}
with open(FILE_NAME, "r") as file:
	reader = csv.reader(file)
	header = next(reader)

	with open("fixed_" + FILE_NAME, "w") as fixed_file:
		writer = csv.writer(fixed_file, lineterminator="\n")
		writer.writerow(header)
		
		for row in reader:
			player_name = row[0]
			if player_name not in seen_players:
				seen_players[player_name] = True
				if row[AVG_POS] == "NULL":
					row[AVG_POS] = str(avg)
				writer.writerow(row)
