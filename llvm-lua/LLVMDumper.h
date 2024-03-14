/*
  Copyright (c) 2009 Robert G. Jakabosky
  
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
  
  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.

  MIT License: http://www.opensource.org/licenses/mit-license.php
*/

#ifndef LLVMDUMPER_h
#define LLVMDUMPER_h

#include "llvm/IR/Module.h"
#include "lua_core.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "lobject.h"

#ifdef __cplusplus
}
#endif

namespace llvm {
class Module;
class Type;
class StructType;
class FunctionType;
class Constant;
class GlobalVariable;
}

class LLVMCompiler;

class LLVMDumper {
private:

	LLVMCompiler *compiler;

public:
	explicit LLVMDumper(LLVMCompiler *compiler);

	void dump(const char *output, lua_State *L, Proto *p, int stripping);

private:
	llvm::Constant* GetConstPtr(llvm::LLVMContext& context, llvm::Constant *val);
	llvm::Constant* GetGlobalStr(llvm::LLVMContext& context, llvm::Module* module, const char *str);

	llvm::GlobalVariable* BuildConstants(llvm::LLVMContext& context, llvm::Module* module, VMModule& vm, Proto *p);
	llvm::GlobalVariable *BuildLocVars(llvm::LLVMContext& context, llvm::Module* module, VMModule& vm, Proto *p);
	llvm::GlobalVariable *BuildUpValues(llvm::LLVMContext& context, llvm::Module* module, Proto *p);
	llvm::Constant* BuildProtoGLobalVar(llvm::LLVMContext& context, llvm::Module* module, VMModule& vm, Proto *p);

	void BuildStandalone(llvm::LLVMContext& context, llvm::Module* module, VMModule& vm, Proto *p);
	void BuildLuaModule(llvm::LLVMContext& context, llvm::Module* module, VMModule& vm, Proto *p, std::string mod_name);

	std::string NormalizeModuleName(std::string mod_name);
};
#endif

