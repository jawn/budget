class Category
	attr_reader :name, :type, :parent, :budgeted_amount, :actual_amount, :sub_categories, :pct_deductible
	def initialize(name, type, parent, amount, pct_deductible = 0.0)
		@name = name
		@budgeted_amount = amount
		@actual_amount = 0
		@type = type
		@parent = parent
		@sub_categories = []
		if(! @parent.nil?)
			@parent.sub_categories << self
			@parent.add_to_budget(@budgeted_amount)
		end
		@pct_deductible = pct_deductible
	end

	def reset_actual_amount
		@actual_amount = 0
	end
	def add_to_budget(amount)
		@budgeted_amount +=amount
	end

	def spend (amount)
		@actual_amount += amount
		if @parent != nil 
			@parent.spend(amount)
		end
	end



end