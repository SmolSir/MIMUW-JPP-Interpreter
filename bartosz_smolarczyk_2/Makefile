source_path = src
output_path = build
grammar_path = gen
flags = -Wall -Wno-unused-imports
output = -outputdir $(output_path)
executable_name = interpreter
includes = -i$(source_path) -i$(grammar_path)

.PHONY :
	all clean

all :
	$(MAKE) clean
	mkdir build
	ghc $(flags) --make $(output) -o $(executable_name) $(includes) Main.hs

clean :
	rm -rf build interpreter
