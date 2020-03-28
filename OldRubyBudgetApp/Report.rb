require "./ReportLine.rb"

class BudgetReport
	attr_reader :minDate, :maxDate, :expensesPeriod, :reportLines, :total_budgeted, :total_expenses, :total_remainder, :total_deductible
	attr_accessor :taxExpensesOnly
	attr_accessor :filter
	attr_accessor :topLevel
	def initialize (minDate, maxDate,categories,expenses,topLevel,filter)
		@minDate = minDate
		@maxDate = maxDate
		@categories = categories
		@expenses = expenses
		calculateExpensesPeriod
		@filter = filter
		@taxExpensesOnly = false
		@topLevel = topLevel
		
		fillReportLines
	end	

	def fillReportLines
		@total_budgeted = 0
		@total_expenses = 0
		@total_remainder= 0
		@total_deductible = 0
		@total_avg_monthly_spend = 0
		@reportLines = []
		if @filter
			categoriesToReport = @categories.each_value.select { |category| 
										@filter.find_index { |id| id == category.name } }
		else	
			categoriesToReport = @categories.each_value
		end
		
		categoriesToReport.each.select { |category| category.type == "Top" }.each { |category|
			line = ReportLine.new(category.name,  
										   category.budgeted_amount * @expensesPeriod, 
										   category.actual_amount,
										   category.actual_amount * category.pct_deductible,
										   category.actual_amount / @expensesPeriod)
			@reportLines << line
			@total_budgeted += line.budgeted
			@total_expenses += line.spent
			@total_remainder+= line.remainder
			@total_deductible+=line.deductible
			@total_avg_monthly_spend+=line.avgmonthlyspent   
			if not @topLevel
				category.sub_categories.each { |sub_category| 
					@reportLines << ReportLine.new(sub_category.name,  
												   sub_category.budgeted_amount * @expensesPeriod, 
												   sub_category.actual_amount,
												   sub_category.actual_amount * sub_category.pct_deductible,
												   sub_category.actual_amount / @expensesPeriod)
				}
			end
		}

	end

	def calculateExpensesPeriod ()


		@expensesPeriod = (@maxDate.year * 12 + @maxDate.month) - (@minDate.year * 12 + @minDate.month) + 1
		# e.g. 2016*12 + 3 = 24195  , 2015*12 + 11 = 24191, 24195-24191 == 4 + 1 = five months from nov 2015 to mar 2016
	end

	def print(topLevelOnly, output = $stdout)
		total_budgeted = 0
		total_expenses = 0

		printf(output, "%50s\n", "From " + @minDate.to_s + " to " + @maxDate.to_s + " for a total of " + @expensesPeriod.to_s + ' months.')
		printf(output, "  %50s %14s %14s %14s %14s %14s\n", "Category", "Budgeted","Actual","Remainder","Tax Deduction","Monthly Average")
		printf(output, "  %50s %14s %14s %14s %14s %14s\n", "========", "========","======","=========","=============","===============")
		@reportLines.each { |line|
		 	printf(output, "  %50s %14.2f %14.2f %14.2f %14.2f %14.2f\n",
		 		line.categoryName, line.budgeted, line.spent, line.remainder ,line.deductible, line.avgmonthlyspent )
		 }
		 printf(output, "  %50s %14.2f %14.2f %14.2f %14.2f %14.2f\n",
		 		"Totals:", @total_budgeted, @total_expenses, @total_remainder, @total_deductible,@total_avg_monthly_spend )





		# categoriesToReport.each { |category| 
		# 	if category.type == "Top" 
		#   		if taxExpensesOnly #refactor this name
		#   			taxFactor = category.pct_deductible
		#   		else			  			
		#   			taxFactor = 1.0
		# 	 	end
				
		# 		printf(output, "  %50s %14.2f %14.2f %14.2f\n", category.name, 
		# 														category.budgeted_amount * @expensesPeriod, 
		# 														category.actual_amount*taxFactor, 
		# 														(category.budgeted_amount * @expensesPeriod)+category.actual_amount*taxFactor)
		# 		total_budgeted += category.budgeted_amount * @expensesPeriod
		# 		total_expenses += category.actual_amount*taxFactor
		# 		if(!topLevelOnly)
		# 			category.sub_categories.each { |category| 
		# 				printf(output, "* %50s %14.2f %14.2f %14.2f\n", 
		# 														category.name, 
		# 														category.budgeted_amount * @expensesPeriod, 
		# 														category.actual_amount*taxFactor,
		# 														(category.budgeted_amount * @expensesPeriod)+category.actual_amount*taxFactor)
		# 			}
		# 			end
		# 	end
			
		# }
		#printf("                           TOTAL %14.2f %14.2f\n", total_budgeted, total_expenses)

		#printf(output, "  %50s %14.2f %14.2f %14.2f\n", " TOTAL", total_budgeted, total_expenses, total_budgeted+total_expenses)
	end

end