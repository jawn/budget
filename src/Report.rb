#!/usr/bin/env ruby
require 'csv'

# this is a ruby script file calculate expenses, totaled by category

# example:
#  ./Report.rb ./data/BusinessChecking.csv' "Business Checking"
#
# This will import the expenses (with account given as 2d parameter)
# with second column (account) equals to: Business Chekcing

# the Report class takes care of reporting totals by category 
class Report
	attr_reader :lines

	def initialize()
        @totals = Hash.new # e.g. { "Household" => 230.00, "Automobile" => "189.00" ... }
		@lines = []
	end		

    # add an expense to the totals, creating a total for the category if its not present yet 
    # then update the report lines
	def addExpense(expense)
		category = expense.category
		amount   = expense.amount
		if !@totals.has_key?(category)
			@totals[category] = amount
		else
			@totals[category] = @totals[category] + amount
		end
		updateLines()
			
	end	
    
    # recreate the report lines, one line per category
	def updateLines() 
		@lines = []
		@totals.each { |cat, amt|
			@lines << ("%-29s:%11.2f" % [cat, amt])	
		}
	end

  # fields in expense csv files
  STATUS=0
  DATE=2
  NOTES=3
  DESCRIPTION=4
  CATEGORY=5
  AMOUNT=6
    def importFromBankCSV (fileName, account)
      expenses=CSV.read(fileName)[0..-1]
      expenses.each do |row|
        expenseDate = 	[]
        status = 		row[STATUS]
        if row[DATE].nil?
          raise "date not found:#{row.to_s}" 
        end 
        expenseDate = 	row[DATE].split("/")
        year = expenseDate[2].to_i
        if year < 100 
          year += 2000 
        end
        month = expenseDate[0].to_i
        day   = expenseDate[1].to_i
        date = 			Date.new(year, month, day)
        description = 	row[DESCRIPTION]
        category = row[CATEGORY]
        bankAmount = row[AMOUNT]
        if bankAmount[0,2] == "--" 
          bankAmount = bankAmount[2..-1]
        end
        amount = 		Float(bankAmount)

        notes = 		row[NOTES] 
        addExpense(Expense.new(status,account,date,description, category,amount,notes))
      end
    end
end


