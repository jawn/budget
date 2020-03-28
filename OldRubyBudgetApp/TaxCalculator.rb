
class Bracket
	attr :minValue, :maxValue, :percentage

	def initialize(minValue, maxValue, percentage)
		@minValue = minValue
		@maxValue = maxValue
		@percentage = percentage
	end

end
class TaxCalculator 

	def initialize()
		@taxTableSingle = []
		@taxTableSingle << Bracket.new(0, 9275, 0.10)
		@taxTableSingle << Bracket.new(9275, 37650, 0.15)
		@taxTableSingle << Bracket.new(37650, 91150, 0.25)
		@taxTableSingle << Bracket.new(91150, 190150, 0.28)
		@taxTableSeparated = []
		@taxTableSeparated << Bracket.new(0, 9275, 0.10)
		@taxTableSeparated << Bracket.new(9275, 37650, 0.15)
		@taxTableSeparated << Bracket.new(37650, 75950, 0.25)
		@taxTableSeparated << Bracket.new(75950, 115725, 0.28)
		@tables = {
			"single"    => @taxTableSingle,
			"marriedSeparate" => @taxTableSeparated
		}
	end

	
	def calculate(taxableEarnings, filingStatus)
		earning = taxableEarnings
		taxAmount = 0
		taxTable = @tables[filingStatus] # first find the table corresponding to filingstatus
		taxTable.each do |bracket|
			if earning >= bracket.maxValue # we are above this bracket: add the amount for this bracket
				taxAmount += (bracket.maxValue - bracket.minValue) * bracket.percentage
			else # we are in this bracket : calculate tax on the remaining
				taxAmount += (earning - bracket.minValue) * bracket.percentage
				return taxAmount
			end
		end

	end
end