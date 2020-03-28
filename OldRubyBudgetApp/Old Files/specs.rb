require "./CategoryAndAmount.rb"

describe "Our price calcutator" do
	before(:each) do
		@calculator = Calculator.new()
		@calculator.input_price(100)
		@calculator.input_quantity(1)
		@calculator.input_state("CA")
	end	

	it "computes a price for 1 item given no discount and standard CA taxes" do
		@calculator.compute_price()
		expect( @calculator.result).to be 108.25 
	end


end

