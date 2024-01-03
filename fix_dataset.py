
## Python script that fixes the average ping for the given csv file.

# Imports
import csv


# Constants
FILE_NAME = "data_analysis.csv"
AVG_POS = 6



# Get avg ping
avg = 0
count = 0
with open(FILE_NAME, "r") as file:
	reader = csv.reader(file)
	next(reader)

	for row in reader:
		if row[AVG_POS] != "NULL":
			avg += float(row[AVG_POS])
			count += 1
avg /= count
print("Final average ping: " + str(avg))

# Fix csv file
with open(FILE_NAME, "r") as file:
	reader = csv.reader(file)

	with open("fixed_" + FILE_NAME, "w") as fixed_file:
		writer = csv.writer(fixed_file, lineterminator="\n")
		for row in reader:
			if row[AVG_POS] == "NULL":
				row[AVG_POS] = str(avg)
			writer.writerow(row)


