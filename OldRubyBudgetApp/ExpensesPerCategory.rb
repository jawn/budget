require 'csv'
require "./Category.rb"
require "./Expense.rb"
#refactor to use the dateFrom String within ImportExpenseCSV

def dateFromString(s)
	dateParts = 	s.split("/")
	year = dateParts[2].to_i
	if year < 100 
		year += 2000 
	end
	month = dateParts[0].to_i
	day   = dateParts[1].to_i
	Date.new(year, month, day)
end

# fields in expense csv files
ACCOUNT=0
DATE=2
NOTES=3
DESCRIPTION=4
CATEGORY=5
AMOUNT=6

def ImportExpenseCSV (fileName, categories)
	container = []

	expenses=CSV.read(fileName)[0..-1]
	expenses.each do |row|
		expenseDate = 	[]
		account = 		row[ACCOUNT]
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
		categoryName = row[CATEGORY]
		category = 		categories[categoryName]
		if category.nil?
			raise "category not found:#{categoryName}" 
		end 



 		amount = 		Float(row[AMOUNT])
		
		notes = 		row[NOTES] 
		expense = Expense.new(account,date,description, category,amount,notes)
		container <<  expense
		category.spend(expense.amount)
	end

	container
end

def selectExpenses(expenses, minDate, maxDate)
	
	expenses.select{ |expense| 
			expense.date >=minDate && expense.date <= maxDate}
end

# fields in category csv files
NAME=0
TYPE=1
PARENT_NAME=2
CATEGORY_AMOUNT=3
ALLOWABLE=4
def ImportCategoryCSV (fileName)
	container = { }
	categories=CSV.read(fileName)
	categories.each  do |row|
		name = row[NAME]
		type = row[TYPE]
		parent_name = row[PARENT_NAME]
		if type == "Sub"
			parent = container[parent_name]
		else
			parent = nil
		end
		amount = row[CATEGORY_AMOUNT].to_f
		allowable = row[ALLOWABLE].to_f
		#if allowable == 0
		#	allowable = 1
		#end
		category = Category.new(name,type,parent,amount,allowable) 
		container[name] = category
	end
	container
end

def updateCategoriesActualAmount(categories, expenses)
	categories.each_value { |category| 
		category.reset_actual_amount 
	}
	expenses.each { |expense| 
		category = expense.category
		category.spend(expense.amount)

	}
	
end


