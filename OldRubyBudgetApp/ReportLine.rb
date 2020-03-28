class ReportLine
	attr_reader :categoryName, :budgeted, :spent, :remainder ,:deductible, :avgmonthlyspent
	def initialize(name, budgeted, spent, deductible,avgmonthlyspent)
		@categoryName = name
		@budgeted = budgeted
		@spent = spent
		@remainder = budgeted - spent 
		@deductible = deductible
		@avgmonthlyspent = avgmonthlyspent
	end
end