class Report
	attr_reader :lines

	def initialize()
		@amount = 0
		@category = ""
		@lines = [""]
	end		

	def addExpense(expense)
		@category = expense.category
		@amount = @amount + expense.amount
		@lines[0] = "%-29s:%11.2f" % [@category, @amount]	
	end	

end
