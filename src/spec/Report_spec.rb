require "./Report.rb"
require "csv"
describe "A Report"  do
	it "should sum its amounts per category for one category" do
		report = Report.new()
		exp1 = Expense.new("posted","checking", Date.new(2020, 02, 13), "Bodies in Motion", "Business Miscellaneous", 30.00, "whatever notes I want")
        exp2 = Expense.new("posted","checking", Date.new(2020, 02, 15), "Bikes in Motion", "Business Miscellaneous",  170.00, "whatever notes I want")
        report.addExpense(exp1)
		report.addExpense(exp2)
		expect(report.lines[0]). to eq "Business Miscellaneous       :     200.00"
	end

	it "should sum its amounts per category for two categories" do
		report = Report.new()
		exp1 = Expense.new("posted","checking", Date.new(2020, 02, 13), "Bodies in Motion", "Business Miscellaneous",  30.00, "whatever notes I want")
        exp2 = Expense.new("posted","checking", Date.new(2020, 02, 15), "Bikes in Motion", "Household Maintenance",  170.00, "whatever notes I want")
		report.addExpense(exp1)
		report.addExpense(exp2)
		result = report.lines.sort()
		expect(result[0]). to eq "Business Miscellaneous       :      30.00"
		expect(result[1]). to eq "Household Maintenance        :     170.00"
	end

    it "should import expenses from the bank data csv file" do
      report = Report.new
      report.importFromBankCSV('../data/appdata/BusinessChecking.csv', "BusinessChecking")
      expect(report.lines.size). to eq 4
      expect(report.lines[0]). to eq "Online Services              :     -12.00"
      expect(report.lines[1]). to eq "Training                     :   -1242.26"
      expect(report.lines[2]). to eq "Credit Card Payments         :     -26.99" 
      expect(report.lines[3]). to eq "Transfers                    :   -1166.74"
    end 
end
