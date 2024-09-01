.SUFFIXES:
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

# OptSpeed or OptSize
OPT = OptSpeed

# Real64 or Real32
Real = Real64

ifdef MSYSTEM
	PRG = .exe
	SYS = Win
	RTS = /c/EigenCompilerSuite/runtime/win64api.obf
else
	PRG = 
	SYS = Lin
	RTS = 
endif

CLS = DBSQLite3Dll$(SYS).cpp
OLS += Const Config$(SYS) Type Char ArrayOfChar$(OPT) OSHost$(SYS) Integer Cardinal $(Real)
OLS += ArrayOfByte$(OPT) ArrayOfSet DateTime String Regex ADTBasicType ADTStream ADTList ADTVector
OLS += ADTSet ADTDictionary ADTTree O2Testing O2Timing$(SYS)
OLS += OS OSStream OSFile OSDir OSPath
MOD = $(addprefix src/, $(addprefix Std., $(addsuffix .mod, $(OLS))))
OBF = $(addprefix build/, $(addprefix Std., $(addsuffix .obf, $(OLS))))

OTS  = TestArrayOfByte TestArrayOfChar TestArrayOfSet TestCardinal TestInteger TestReal
OTS += TestString TestRegex TestDateTime TestADTBasicType TestADTList TestADTSet
OTS += TestADTDictionary TestADTVector TestADTTree TestADTStream TestOSPath TestOS

TMOD = $(addprefix tests/, $(addsuffix .mod, $(OTS)))
TOBF = $(addprefix build/, $(addsuffix .obf, $(OTS)))

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
build/Std.Regex.obf : src/Std.ArrayOfChar$(OPT).mod src/Std.ArrayOfSet.mod
build/Std.String.obf : src/Std.Char.mod src/Std.Config$(SYS).mod src/Std.Type.mod src/Std.ArrayOfChar$(OPT).mod src/Std.Integer.mod src/Std.Cardinal.mod src/Std.DateTime.mod

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
build/TestRegex.obf : src/Std.Regex.mod
build/TestString.obf : src/Std.String.mod

build/%.obf: tests/%.mod
	@echo compiling $<
	@mkdir -p build
	@cd build && cat $(addprefix ../, $<) | awk '{gsub("__LINE__",NR,$$0);print}' > $(notdir $<)
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
	@./$@

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

.PHONY: install
install: std.lib
	@echo Install
	@cp -f std.lib /c/EigenCompilerSuite/runtime/
	@cp -f build/std.*.sym /c/EigenCompilerSuite/libraries/oberon/

.PHONY: clean
clean:
	@echo Clean
	@-rm -rf build
	@-rm TestMain$(PRG) Test$(PRG) perfLength$(PRG) perfIndex$(PRG) perfFillChar$(PRG) perfCompare$(PRG)