require "./TaxCalculator.rb"

describe "a single filer tax calculation" do
	before(:each) do
    	
  	end
	it "should calculate my taxes owed when I'm single and earning less than 9275" do 
		taxCalculator = TaxCalculator.new()
		expect(taxCalculator.calculate(5000, "single")).to eq 500
		expect(taxCalculator.calculate(6000, "single")).to eq 600	
	end
	it "should calculate my taxes owed when I'm single and earning between 9275 and 37650" do 
		taxCalculator = TaxCalculator.new()
		expect(taxCalculator.calculate(10000, "single")).to eq 1036.25
		expect(taxCalculator.calculate(12000, "single")).to eq 1336.25
	end
	it "should calculate my taxes owed when I'm single and earning between 37650 and 91150" do 
		taxCalculator = TaxCalculator.new()
		expect(taxCalculator.calculate(45000, "single")).to eq 7021.25
		expect(taxCalculator.calculate(80000, "single")).to eq 15771.25		
	end
	it "should calculate my taxes owed when I'm single and earning between 91150 and 190150" do 
		taxCalculator = TaxCalculator.new()
		expect(taxCalculator.calculate(100000, "single")).to eq 21036.75

	end

	it "should calculate my taxes owed when I'm married filing seperately and earning less than 9275" do 
		taxCalculator = TaxCalculator.new()
		expect(taxCalculator.calculate(5000, "marriedSeparate")).to eq 500
		expect(taxCalculator.calculate(6000, "marriedSeparate")).to eq 600	
	end
	it "should calculate my taxes owed when I'm married filing separately and earning between 9275 and 37650" do 
		taxCalculator = TaxCalculator.new()
		expect(taxCalculator.calculate(10000, "marriedSeparate")).to eq 1036.25
		expect(taxCalculator.calculate(12000, "marriedSeparate")).to eq 1336.25
	end
	it "should calculate my taxes owed when I'm married filing separately and earning between 37650 and 75950" do 
		taxCalculator = TaxCalculator.new()
		expect(taxCalculator.calculate(45000, "marriedSeparate")).to eq 7021.25
	end
	it "should calculate my taxes owed when I'm married filing separately and earning between 75950 and 115725" do 
		taxCalculator = TaxCalculator.new()
		expect(taxCalculator.calculate(80000, "marriedSeparate")).to eq 15892.75
		expect(taxCalculator.calculate(100000, "marriedSeparate")).to eq 21492.75

	end
end 