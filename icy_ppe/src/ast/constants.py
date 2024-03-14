f = open("constants.dat", "r")
for line in f:
	split = line.split(",")	
	print("     #[token(\""+split[0].strip()+"\", |_| Constant::Builtin(&BuiltinConst::"+split[0].strip()+"), ignore(case))]") 
print("------------------")
f = open("constants.dat", "r")
for line in f:
	split = line.split(",")	
	print("        m.insert(unicase::Ascii::new(\""+split[0].strip()+"\".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::"+split[0].strip()+")));") 

#	print("    pub const " + split[0].strip() +": BuiltinConst = BuiltinConst::new(\""+ split[0].strip() + "\", "+ split[1].strip() +");") 
