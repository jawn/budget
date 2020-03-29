require "./Category.rb"
require "./Expense.rb"

require "Date"
describe "Expense"  do 
	it "should know its Account,Date,Description,Category,Amount,Notes" do
	 	category = Category.new("Groceries","top",nil, 300)
		expense = Expense.new("Costco", Date.new(2016,04,13) , "safeway", category, 14.07, "dog food" )
		expect(expense.account).to eq "Costco"
		expect(expense.date).to eq Date.new(2016,04,13)
		expect(expense.description).to eq "safeway"
		expect(expense.amount).to eq 14.07
		expect(expense.notes).to eq "dog food"
	end
	
end
