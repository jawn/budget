# the Report class takes care of reporting totals by category 
class Report
	attr_reader :lines

	def initialize()
		@amount = 0
		@totals = Hash.new
		@lines = [""]
	end		

	def addExpense(expense)
		category = expense.category
		amount   = expense.amount
		if !@totals.has_key?(category)
			@totals[category] = amount
		else
			@totals[category] = @totals[category] + amount
		end
		updateLines()
		print @totals
			
	end	
	def updateLines() 
		@lines = []
		@totals.each { |cat, amt|
			@lines << ("%-29s:%11.2f" % [cat, amt])	
		}
	end

end
