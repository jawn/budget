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

end
