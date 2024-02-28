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

#ifndef LLVMCOMPILER_h
#define LLVMCOMPILER_h

#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"

#include "VMModule.h"

#include "lua_core.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "lobject.h"

#include "lua_vm_ops.h"

#ifdef __cplusplus
}
#endif

namespace llvm {
class ExecutionEngine;
class Timer;
  namespace legacy {
    class FunctionPassManager;
  }
}

class LLVMCompiler {
private:
	class OPValues {
	private:
		std::vector<llvm::Value*> values;
	
	public:
		explicit OPValues(int len)
		{
			values.resize(len, nullptr);
		}
	
		void set(int idx, llvm::Value *val) {
			assert(idx >= 0 && idx < values.size());
			values[idx] = val;
		}
		llvm::Value *get(int idx) {
			assert(idx >= 0 && idx < values.size());
			return values[idx];
		}
	};

	struct BuildContext
	{
		Instruction* code = nullptr;
		TValue* k = nullptr;
		int code_len;
		int strip_ops = 0;

		llvm::Value* func_L = nullptr;
		llvm::CallInst* func_cl = nullptr;
		llvm::CallInst* func_k = nullptr;
	};

private:
  VMModule vm_module;
  llvm::orc::ThreadSafeContext ts_context;
	std::unique_ptr<llvm::orc::LLJIT> jit;

	bool strip_code;

	// count compiled opcodes.
	int *opcode_stats;

	// timers
	llvm::Timer *lua_to_llvm;
	llvm::Timer *codegen;

	// opcode hints/values/blocks/need_block arrays used in compile() method.
	std::vector<hint_t> op_hints;
	std::vector<std::unique_ptr<OPValues>> op_values;
	std::vector<llvm::BasicBlock*> op_blocks;
	std::vector<bool> need_op_block;

	std::unordered_map<Proto*, llvm::orc::ResourceTrackerSP> trackers;

	// resize the opcode hint data arrays.
	void ResizeOpcodeData(int code_len);
	// reset/clear the opcode hint data arrays.
	void ClearOpcodeData(int code_len);

	void FindBasicBlockPoints(llvm::LLVMContext& context, llvm::IRBuilder<>& builder, BuildContext& bcontext);
	void PreCreateBasicBlocks(llvm::LLVMContext& context, llvm::Function* func, BuildContext& bcontext);
	std::vector<llvm::Value*> GetOpCallArgs(llvm::LLVMContext& context, const vm_func_info* func_info, BuildContext& bcontext, int i);
	void InsertDebugCalls(llvm::LLVMContext& context, llvm::IRBuilder<>& builder, BuildContext& bcontext, int i);
public:
	explicit LLVMCompiler(int useJIT);
	~LLVMCompiler();

	/*
	 * set code stripping mode.
	 */
	void setStripCode(bool strip) {
		strip_code = strip;
	}

	/*
	 * return the module.
	 */
//	llvm::Module* getModule() {
//		return ModuleRaw;
//	}
//
//	llvm::LLVMContext& getCtx() {
//		return Context;
//	}
//
//	llvm::FunctionType *get_lua_func_type() {
//		return lua_func_type;
//	}

	llvm::Type *get_var_type(val_t type, hint_t hints);

	llvm::Value *get_proto_constant(TValue *constant);

  std::string GenerateFunctionName(Proto *p);
	
	/*
	 * Pre-Compile all loaded functions.
	 */
	void compileAll(lua_State *L, Proto *parent);

	void compile(lua_State *L, Proto *p);

	void free(lua_State *L, Proto *p);
};

#endif

