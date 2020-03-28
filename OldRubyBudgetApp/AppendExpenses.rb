#!/usr/bin/env ruby
require 'csv'

# this is a ruby script file to append downloaded expenses files to the main expenses CSV file

# example:
#  ./AppendExpenses.rb './TestData/Personal Checking.csv' './TestData/2016 Budget Transactions.csv'
#
# This will append the P.C csv expenses to the 2016 B T. csv file,
# with first column (account) equals to: Personal Chekcing

# check that two arguments are given
if ARGV.count < 2
	puts "usage: ./AppendExpenses downloaded.csv mainexpenses.csv"
	exit
end

# determine the files names from args passed to the script
downloadedCSVname = ARGV[0]
mainCSVname =       ARGV[1]

# extract the account label from the first filename (remove path and extension from that filename)
accountLabel = File.basename(downloadedCSVname, ".csv") 

puts "I'm going to work with this label:#{accountLabel}"

# read the dowloaded expense CSV
downloadedCSV = CSV.read(downloadedCSVname)
puts "It's a #{downloadedCSV.size} lines file."


# replace each 1st column of the CSV with account name, for all rows of the CSV
downloadedCSV.each do |row|
	row[0] = accountLabel
end

# open the main expenses CSV and look for the first row of downloaded csv, if found, abort with message
mainexpensesCSV = CSV.read(mainCSVname)
firstRowDownloaded = downloadedCSV[0]

duplicates = mainexpensesCSV.select do |row|
	row == firstRowDownloaded
end 

if duplicates.size > 0
	puts "the line #{firstRowDownloaded} is already present in the main expenses file. Aborting."
	exit 
 end

# save this CSV again
 CSV.open(downloadedCSVname, "wb") do |csvOut|
	downloadedCSV.each do |row|
		if row.count > 1
			csvOut << row
		end
	end
end


# backup the main file
cmd = "cp '#{mainCSVname}' '#{mainCSVname}.bak'"
system(cmd)

# add the downloadedCSV to the mainexpenseCSV
CSV.open(mainCSVname, "wb") do |csvOut|
	# first add all the rows from mainExpense
	mainexpensesCSV.each do |row|
		csvOut << row
	end
	# second, add all the rows from downloadedCsv
	downloadedCSV.each do |row|
		csvOut << row
	end
end



