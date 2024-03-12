f = open("FUNCS", "r")
#for line in f:
#	print("hint-func-"+ line.strip().lower()+"=todo")


for line in f:
	print("        OpCode::" + line.strip() + " => get_hint(fl!(crate::LANGUAGE_LOADER, \"hint-func-"+ line.strip().lower()+"\")),")
