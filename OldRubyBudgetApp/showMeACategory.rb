#!/usr/bin/env ruby
load 'Report.rb'
load 'ExpensesPerCategory.rb'
load 'DetailedExpenseReport.rb'

if ARGV.count < 1
	puts "usage: ./showMeACategory.rb <Category>"
	exit
end
category = ARGV[0]
categories = ImportCategoryCSV("./Data/Categories.csv")
expenses = ImportExpenseCSV("./Data/Budget Transactions.csv", categories)

report = DetailedExpenseReport.new(expenses, category)
puts report.output