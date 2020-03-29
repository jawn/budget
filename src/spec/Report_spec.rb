require "./Report.rb"
describe "A Report"  do
	it "should sum its amounts per category" do
		report = Report.new()
		exp1 =Expense.new("checking",Date.new(2020,02,13),"Bodies in Motion","Business Miscellaneous", 30.00,"whatever notes I want")
		report.addExpense(exp1)
		report.addExpense(Expense.new("checking",Date.new(2020,02,15),"Bikes in Motion","Business Miscellaneous", 170.00,"whatever notes I want"))
		expect(report.lines[0]). to eq "Business Miscellaneous       :     200.00"
	end
end
