.SUFFIXES:
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

# OptSpeed or OptSize
OPT = OptSpeed

# Real64 or Real32
Real = Real64

ifdef MSYSTEM
	PRG = .exe
	SYS = Win
	DESTDIR = /c/EigenCompilerSuite/
	RTS = $(DESTDIR)runtime/win64api.obf
	CONV = unix2dos
else
	PRG = 
	SYS = Lin
	DESTDIR = ~/.local/lib/ecs/
	RTS = 
	CONV = dos2unix
endif

OLS += Const Config$(SYS) Type Char ArrayOfChar$(OPT) OSHost$(SYS) Integer Cardinal $(Real)
OLS += ArrayOfByte$(OPT) ArrayOfSet DateTime String StringPattern ADTBasicType ADTStream ADTList ADTVector
OLS += ADTPair ADTSet ADTDictionary ADTTree O2Testing O2Timing$(SYS)
OLS += OS OSStream OSFile OSDir OSPath DataConfig DataLZ4
MOD = $(addprefix src/, $(addprefix Std., $(addsuffix .mod, $(OLS))))
OBF = $(addprefix build/, $(addprefix Std., $(addsuffix .obf, $(OLS))))

OTS  = TestArrayOfByte TestArrayOfChar TestArrayOfSet TestCardinal TestInteger TestReal
OTS += TestString TestStringPattern TestDateTime TestADTBasicType TestADTList TestADTSet
OTS += TestADTDictionary TestADTVector TestADTTree TestADTStream TestOSPath TestOS
OTS += TestDataConfig TestDataLZ4

TMOD = $(addprefix tests/, $(addsuffix .mod, $(OTS)))
TOBF = $(addprefix build/, $(addsuffix .obf, $(OTS)))

DOC = ADTBasicType ADTDictionary ADTList ADTPair ADTSet ADTStream ADTTree ADTVector ArrayOfByte
DOC += ArrayOfChar ArrayOfSet Cardinal Char Config Const DataConfig DataLZ4 DateTime Integer
DOC += O2Testing O2Timing OS OSDir OSFile OSHost OSPath OSStream Real String StringPattern Type

DRST = $(addprefix doc/src/Std., $(addsuffix .mod.rst, $(DOC)))

.PHONY: all
all : std.lib

build/Std.ADTBasicType.obf : src/Std.Type.mod
build/Std.ADTDictionary.obf : src/Std.ADTVector.mod
build/Std.ADTSet.obf : src/Std.ADTVector.mod
build/Std.ADTStream.obf : src/Std.ArrayOfByte$(OPT).mod src/Std.ArrayOfChar$(OPT).mod src/Std.Cardinal.mod src/Std.Config$(SYS).mod src/Std.Const.mod src/Std.DateTime.mod src/Std.Integer.mod src/Std.String.mod src/Std.Type.mod
build/Std.ADTVector.obf : src/Std.ArrayOfByte$(OPT).mod src/Std.Cardinal.mod
build/Std.ArrayOfChar$(OPT).obf : src/Std.Char.mod src/Std.Const.mod src/Std.Type.mod
build/Std.ArrayOfSet.obf : src/Std.ArrayOfByte$(OPT).mod
build/Std.Cardinal.obf : src/Std.Const.mod src/Std.Type.mod src/Std.Char.mod src/Std.OSHost$(SYS).mod
build/Std.DateTime.obf : src/Std.Const.mod src/Std.Type.mod src/Std.Char.mod src/Std.Integer.mod src/Std.OSHost$(SYS).mod src/Std.ArrayOfChar$(OPT).mod
build/Std.Integer.obf : src/Std.Const.mod src/Std.Type.mod src/Std.Char.mod
build/Std.$(Real).obf : src/Std.Const.mod src/Std.Type.mod src/Std.Char.mod src/Std.ArrayOfChar$(OPT).mod
build/Std.O2Testing.obf : src/Std.ArrayOfChar$(OPT).mod
build/Std.OS.obf : src/Std.Char.mod src/Std.ArrayOfChar$(OPT).mod src/Std.String.mod src/Std.OSHost$(SYS).mod
build/Std.OSDir.obf : src/Std.String.mod src/Std.OSHost$(SYS).mod
build/Std.OSFile.obf : src/Std.DateTime.mod src/Std.OSHost$(SYS).mod
build/Std.OSPath.obf : src/Std.ArrayOfChar$(OPT).mod src/Std.Config$(SYS).mod src/Std.Const.mod src/Std.Char.mod src/Std.OSDir.mod src/Std.String.mod
build/Std.OSStream.obf : src/Std.Const.mod src/Std.ADTStream.mod src/Std.OSHost$(SYS).mod
build/Std.String.obf : src/Std.Char.mod src/Std.Config$(SYS).mod src/Std.Type.mod src/Std.ArrayOfChar$(OPT).mod src/Std.Integer.mod src/Std.Cardinal.mod src/Std.DateTime.mod
build/Std.StringPattern.obf : src/Std.Char.mod src/Std.ArrayOfChar$(OPT).mod
build/Std.Type.obf : src/Std.Const.mod src/Std.Config$(SYS).mod
build/Std.DataConfig.obf : src/Std.Const.mod src/Std.Char.mod src/Std.ArrayOfChar$(OPT).mod src/Std.String.mod src/Std.Type.mod src/Std.ADTDictionary.mod src/Std.ADTSet.mod 

build/%.obf: src/%.mod
	@echo compiling $< 
	@mkdir -p build
	@cd build && ecsd -c $(addprefix ../, $<)

std.lib : $(OBF)
	@echo linking $@
	@-rm $@
	@touch $@
	@linklib $@ $^

build/TestADTBasicType.obf : src/Std.ADTBasicType.mod
build/TestADTDictionary.obf : src/Std.ADTDictionary.mod
build/TestADTList.obf : src/Std.ADTList.mod
build/TestADTSet.obf : src/Std.ADTSet.mod
build/TestADTTree.obf : src/Std.ADTTree.mod
build/TestADTVector.obf : src/Std.ADTVector.mod
build/TestADTStream.obf : src/Std.Const.mod src/Std.String.mod src/Std.ADTStream.mod
build/TestArrayOfByte.obf : src/Std.ArrayOfByte$(OPT).mod
build/TestArrayOfChar.obf : src/Std.ArrayOfChar$(OPT).mod
build/TestArrayOfSet.obf : src/Std.ArrayOfSet.mod
build/TestCardinal.obf : src/Std.Cardinal.mod
build/TestDateTime.obf : src/Std.DateTime.mod
build/TestInteger.obf : src/Std.Integer.mod
build/TestReal.obf : src/Std.$(Real).mod
build/TestOS.obf : src/Std.String.mod src/Std.OSStream.mod src/Std.OSPath.mod src/Std.OSFile.mod src/Std.OSDir.mod
build/TestOSPath.obf : src/Std.OSPath.mod
build/TestString.obf : src/Std.String.mod
build/TestStringPattern.obf : src/Std.StringPattern.mod src/Std.ArrayOfChar$(OPT).mod
build/TestDataConfig.obf : src/Std.DataConfig.mod src/Std.ADTStream.mod src/Std.String.mod
build/TestDataLZ4.obf : src/Std.DataLZ4.mod src/Std.ArrayOfChar$(OPT).mod

build/%.obf: tests/%.mod
	@echo compiling $<
	@mkdir -p build
	@cd build && cat $(addprefix ../, $<) | awk '{gsub("__LINE__",NR,$$0);print}' | $(CONV) > $(notdir $<)
	@cd build && ecsd -c $(notdir $<)

TestMain$(PRG) : $(TOBF) std.lib
	@echo compiling $<
	@mkdir -p build
	@cd build && cp -f ../tests/Main.mod .
	@cd build && ecsd Main.mod  $(notdir $(TOBF)) ../std.lib $(RTS)
	@cp build/Main$(PRG) TestMain$(PRG)
	@./TestMain$(PRG)

Test$(PRG) : misc/Test.mod std.lib
	@echo compiling $<
	@mkdir -p build
	@cd build && cp -f $(addprefix ../, $<) .
	@cd build && ecsd $(notdir $<) ../std.lib $(RTS)
	@cp build/$@ .

perfLength$(PRG) : misc/perfLength.mod std.lib
	@echo compiling $<
	@mkdir -p build
	@cd build && cp -f $(addprefix ../, $<) .
	@cd build && ecsd $(notdir $<) ../std.lib $(RTS)
	@cp build/$@ .
	@./$@

perfIndex$(PRG) : misc/perfIndex.mod std.lib
	@echo compiling $<
	@mkdir -p build
	@cd build && cp -f $(addprefix ../, $<) .
	@cd build && ecsd $(notdir $<) ../std.lib $(RTS)
	@cp build/$@ .
	@./$@

perfFillChar$(PRG) : misc/perfFillChar.mod std.lib
	@echo compiling $<
	@mkdir -p build
	@cd build && cp -f $(addprefix ../, $<) .
	@cd build && ecsd $(notdir $<) ../std.lib $(RTS)
	@cp build/$@ .
	@./$@

perfCompare$(PRG) : misc/perfCompare.mod std.lib
	@echo compiling $<
	@mkdir -p build
	@cd build && cp -f $(addprefix ../, $<) .
	@cd build && ecsd $(notdir $<) ../std.lib $(RTS)
	@cp build/$@ .
	@./$@

doc/src/Std.ADTBasicType.mod.rst : src/Std.ADTBasicType.mod
	@-mkdir -p doc/src
	./tools/docgen.py $< -o $@

doc/src/Std.ADTDictionary.mod.rst : src/Std.ADTDictionary.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.ADTList.mod.rst : src/Std.ADTList.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.ADTPair.mod.rst : src/Std.ADTPair.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@
	
doc/src/Std.ADTSet.mod.rst : src/Std.ADTSet.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.ADTStream.mod.rst : src/Std.ADTStream.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.ADTTree.mod.rst : src/Std.ADTTree.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.ADTVector.mod.rst : src/Std.ADTVector.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.ArrayOfByte.mod.rst : src/Std.ArrayOfByte$(OPT).mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.ArrayOfChar.mod.rst : src/Std.ArrayOfChar$(OPT).mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.ArrayOfSet.mod.rst : src/Std.ArrayOfSet.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.Cardinal.mod.rst : src/Std.Cardinal.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.Char.mod.rst : src/Std.Char.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.Config.mod.rst : src/Std.Config$(SYS).mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.Const.mod.rst : src/Std.Const.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.DataConfig.mod.rst : src/Std.DataConfig.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.DataLZ4.mod.rst : src/Std.DataLZ4.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.DateTime.mod.rst : src/Std.DateTime.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.Integer.mod.rst : src/Std.Integer.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.O2Testing.mod.rst : src/Std.O2Testing.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.O2Timing.mod.rst : src/Std.O2Timing$(SYS).mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.OS.mod.rst : src/Std.OS.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.OSDir.mod.rst : src/Std.OSDir.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.OSFile.mod.rst : src/Std.OSFile.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.OSHost.mod.rst : src/Std.OSHost$(SYS).mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.OSPath.mod.rst : src/Std.OSPath.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.OSStream.mod.rst : src/Std.OSStream.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.Real.mod.rst : src/Std.$(Real).mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.String.mod.rst : src/Std.String.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.StringPattern.mod.rst : src/Std.StringPattern.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

doc/src/Std.Type.mod.rst : src/Std.Type.mod
	@-mkdir -p doc/src
	@./tools/docgen.py $< -o $@

.PHONY: doc
doc: $(DRST)
	@echo Building doc
	@make -C doc html
	@start "" build/doc/html/index.html &

.PHONY: install
install: std.lib
	@echo Install
	@cp -f std.lib $(DESTDIR)runtime/
	@cp -f build/std.*.sym $(DESTDIR)libraries/oberon/

.PHONY: clean
clean:
	@echo Clean
	@-rm -rf build doc/src
	@-rm -rf doc/src
	@-rm TestMain$(PRG) Test$(PRG) perfLength$(PRG) perfIndex$(PRG) perfFillChar$(PRG) perfCompare$(PRG)
