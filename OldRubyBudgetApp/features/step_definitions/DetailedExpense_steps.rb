load "ExpensesPerCategory.rb"
load "DetailedExpenseReport.rb"


Given(/^the following transactions exist:$/) do |table|
  @transactions = []
  lines = table.raw[1..-1]
  lines.each { |line| 
  	account = line[0]
  	date    = dateFromString(line[1])
  	notes   = line[2]
  	description = line[3]
  	categoryName = line[4]
  	amount = line[5]
    category = Category.new(categoryName, nil, nil, 0)
  	@transactions << Expense.new(account,date,description,category,amount,notes)
  	} 
end

Given(/^I select the category "([^"]*)"$/) do |category|
  @category = category
 end

When(/^I run the report$/) do
	@expenseReport = DetailedExpenseReport.new(@transactions, @category)
end

Then(/^I should see the following data:$/) do |table|
  expected = []
  lines = table.raw[1..-1]
  lines.each { |line| 
  	account = line[0]
  	date    = dateFromString(line[1])
  	notes   = line[2]
  	description = line[3]
  	category = line[4]
  	amount = line[5]
  	expected << Expense.new(account,date,description, category,amount,notes)
  	} 
  	# compare @expenseReport.expenses with expected
  	expect(@expenseReport.expenses.size).to eq expected.size 
  	@expenseReport.expenses.each_index { |n|
  		expect(@expenseReport.expenses[n].date).to eq expected[n].date # wrong comparison method
  		# the data is the same, but in differents objects
  		# we need to compare the content 
  	 }

end
Then(/^the display should look like:$/) do |string|
   expect(@expenseReport.output).to eq string
end



