class DetailedExpenseReport
	attr_reader :expenses
	
	def initialize(expenses, categoryName)
		@categoryName = categoryName
		@expenses = []
		((expenses.select { |expense| 
				expense.category.name == categoryName}
		 ).sort { |expenseA,expenseB|
		 	expenseA.date <=> expenseB.date }  
		 ).each { |expense| 
			@expenses << expense
		}
		@expenses
	end

	def output
		header = 	
      "#{@categoryName}\n" +
      "\n" +
      sprintf("%-20s %-10s %-42s %-25s %14s\n", "Account", "Date", "Notes", "Description", "Amount") +
      sprintf("%-20s %-10s %-42s %-25s %14s\n", "=======", "====", "=====", "===========", "======")
      lines = ""
      grand_total=0

      @expenses.each { |expense| 
      	lines += sprintf("%-20s %02d/%02d/%02d %-42s %-25s %14.2f\n",
      		 expense.account[0..19], 
      		 expense.date.month(), expense.date.day(), expense.date.year(), 
      		 expense.notes.nil? ? "" : expense.notes[0..41],
      		 expense.description[0..24], 
      		 expense.amount)
      	grand_total+=expense.amount
      }
      ruler  = sprintf("%-20s %-10s %-42s %-25s %14s\n", "=======", "====", "=====", "===========", "======")

      footer = sprintf("%-100s %14.2f\n", 
      		  'TOTALS: ',
      		   grand_total)

      header + lines + ruler + footer
	end
end


