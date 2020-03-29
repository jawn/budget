require "./Category.rb"

describe "A category"  do
	it "should know its name, type and amount, and allowable expense factor" do
		category = Category.new("MyItem","top",nil,4807.0)
		expect(category.name).to eq "MyItem"
		expect(category.type).to eq "top"
		expect(category.budgeted_amount).to eq 4807.0
		expect(category.pct_deductible).to eq 0.0
	end
	it "should know its subcategories" do 
		topCategory = Category.new("my top category","top",nil,1000)
		expect(topCategory.sub_categories.length).to eq 0
		subCategoryOne = Category.new("my sub category one","sub",topCategory,300)
		subCategoryTwo = Category.new("my sub category two","sub",topCategory,200)
		expect(topCategory.sub_categories.length).to eq 2
		expect(topCategory.sub_categories[0]).to be subCategoryOne
		expect(topCategory.sub_categories[1]).to be subCategoryTwo		
		
	end
	it "should contain total expenses" do
		category = Category.new("TaxExpenses", "top",nil, 289)
		expect(category.budgeted_amount).to eq 289
		expect(category.actual_amount).to eq 0
		category.spend(120)
		expect(category.actual_amount).to eq 120
		category.spend(83)
		expect(category.actual_amount).to eq 203
	end
	it "should augment expenses on its parent" do
		automotive = Category.new("Automotive Expenses","top",nil,1000)
		toyota = Category.new("MyToyota", "sub", automotive,300)
		honda = Category.new("MyHonda", "sub", automotive, 250)
		expect(automotive.actual_amount).to eq 0
		toyota.spend(150)
		expect(toyota.actual_amount).to eq 150
		expect(automotive.actual_amount).to eq 150
		honda.spend(50)
		expect(automotive.actual_amount).to eq 200
	end
	it "should augment budgeted amount on its parent" do
		automotive = Category.new("Automotive Expenses","top",nil,1000)
		toyota = Category.new("MyToyota", "sub", automotive,300)
		honda = Category.new("MyHonda", "sub", automotive, 250)
		expect(automotive.budgeted_amount).to eq 1000+300+250
	end	
end
