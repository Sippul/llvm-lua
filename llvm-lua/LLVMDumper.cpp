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

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include <string>
#include <vector>
#include <fstream>
#include <stdint.h>

#include "LLVMCompiler.h"
#include "LLVMDumper.h"
#include "lstate.h"
#include "load_jit_proto.h"
#include "load_embedded_bc.h"
#include "liblua_main_bc.h"


static llvm::cl::opt<bool> LuaModule("lua-module",
                   llvm::cl::desc("Generate a Lua Module instead of a standalone exe."),
                   llvm::cl::init(false));

static llvm::cl::opt<bool> NoMain("no-main",
                   llvm::cl::desc("Don't link in liblua_main.bc."),
                   llvm::cl::init(false));

//===----------------------------------------------------------------------===//
// Dump a compilable bitcode module.
//===----------------------------------------------------------------------===//

LLVMDumper::LLVMDumper(LLVMCompiler *compiler_) : compiler(compiler_)
{

}

void LLVMDumper::dump(const char *output, lua_State *L, Proto *p, int stripping) {
	std::error_code error;
  llvm::raw_fd_ostream out(output, error);

	if (!error)
  {
		compiler->SetStripCode(stripping);

		// Internalize all opcode functions.
//		for (llvm::Module::iterator I = Module->begin(), E = Module->end(); I != E; ++I) {
//			llvm::Function *Fn = &*I;
//			if (!Fn->isDeclaration())
//				Fn->setLinkage(llvm::GlobalValue::getLinkOnceLinkage(true));
//		}

		// Compile all Lua prototypes to LLVM IR
		compiler->CompileAll(L, p);
		auto& vm = compiler->GetVMModule();
		auto final_module = compiler->LinkAllModulesIntoOne();
		auto& context = final_module->getContext();

		if (LuaModule)
		{
			// Dump proto info to static variable and create 'luaopen_<mod_name>' function.
			BuildLuaModule(context, final_module.get(), vm, p, output);
		}
		else
		{
			// Dump proto info to global for standalone exe.
			BuildStandalone(context, final_module.get(), vm, p);

			// link with liblua_main.bc
			if (!NoMain)
      {
				auto liblua_main = LoadEmbeddedBitcode(context, "liblua_main_bc", liblua_main_bc, sizeof(liblua_main_bc));


				if (llvm::Linker::linkModules(*final_module, std::move(liblua_main)))
        {
          // TODO: print correct error
					fprintf(stderr, "Failed to link compiled Lua script with embedded 'liblua_main.bc': %s",
						"unknown");
					exit(1);
				}
			}
		}

		llvm::verifyModule(*final_module);
		llvm::WriteBitcodeToFile(*final_module, out);
	}
  else
  {
		fprintf(stderr, "Failed to open output file: %s", error.message().c_str());
		exit(1);
	}
}

llvm::Constant *LLVMDumper::GetConstPtr(llvm::LLVMContext& context, llvm::Constant *val) {
	auto idx_list =
	{
		llvm::Constant::getNullValue(llvm::IntegerType::get(context, 32)),
		llvm::Constant::getNullValue(llvm::IntegerType::get(context, 32))
	};

	return llvm::ConstantExpr::getGetElementPtr(val->getType(), val, idx_list);
}

llvm::Constant *LLVMDumper::GetGlobalStr(llvm::LLVMContext& context, llvm::Module* module, const char *str)
{
	auto str_const = llvm::ConstantDataArray::getString(context, str, true);
	auto var_str = new llvm::GlobalVariable(*module, str_const->getType(), true,
		llvm::GlobalValue::InternalLinkage, str_const, ".str");

	return GetConstPtr(context, var_str);
}

llvm::GlobalVariable *LLVMDumper::BuildConstants(llvm::LLVMContext& context, llvm::Module* module, VMModule& vm, Proto *p)
{
	llvm::GlobalVariable *constant;
	llvm::Constant *array_struct;
	std::vector<llvm::Constant *> array_struct_fields;

	for (int i = 0; i < p->sizek; i++)
	{
		int const_type = 0;
		int const_length = 0;
		llvm::StructType *type;
		std::vector<llvm::Constant *> tmp_struct;
		llvm::Constant* value;
		TValue* tval = &(p->k[i]);
		const_length = 0;
		switch(ttype(tval)) {
			case LUA_TSTRING:
				const_type = TYPE_STRING;
				const_length = tsvalue(tval)->len;
				type = vm.t_constant_str->type;
				tmp_struct.push_back(GetGlobalStr(context, module, svalue(tval)));
				if (vm.t_constant_str->padding != nullptr)
				{
					tmp_struct.push_back(vm.t_constant_str->padding);
				}
				value = llvm::ConstantStruct::getAnon(context, tmp_struct, false);
				break;
			case LUA_TBOOLEAN:
				const_type = TYPE_BOOLEAN;
				type = vm.t_constant_bool->type;
				tmp_struct.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, !l_isfalse(tval))));
				if (vm.t_constant_bool->padding != nullptr)
				{
					tmp_struct.push_back(vm.t_constant_bool->padding);
				}
				value = llvm::ConstantStruct::getAnon(context, tmp_struct, false);
				break;
			case LUA_TNUMBER:
				const_type = TYPE_NUMBER;
				type = vm.t_constant_num->type;
				tmp_struct.push_back(llvm::ConstantFP::get(context, llvm::APFloat(nvalue(tval))));
				if (vm.t_constant_num->padding != nullptr)
				{
					tmp_struct.push_back(vm.t_constant_num->padding);
				}
				value = llvm::ConstantStruct::getAnon(context, tmp_struct, false);
				break;
			case LUA_TNIL:
			default:
				const_type = TYPE_NIL;
				type = vm.t_constant_bool->type;
				tmp_struct.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, 0)));
				if (vm.t_constant_bool->padding != nullptr)
				{
					tmp_struct.push_back(vm.t_constant_bool->padding);
				}
				value = llvm::ConstantStruct::getAnon(context, tmp_struct, false);
				break;
		}

		tmp_struct.clear();
		tmp_struct.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, const_type)));
		tmp_struct.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, const_length)));
		tmp_struct.push_back(value);
		array_struct_fields.push_back(llvm::ConstantStruct::get(type, tmp_struct));
	}

	array_struct = llvm::ConstantStruct::getAnon(context, array_struct_fields, false);
	constant = new llvm::GlobalVariable(*module, array_struct->getType(), true,
		llvm::GlobalValue::InternalLinkage, array_struct, ".constants");

	return constant;
}

llvm::GlobalVariable *LLVMDumper::BuildLocVars(llvm::LLVMContext& context, llvm::Module* module, VMModule& vm, Proto *p) {
	llvm::GlobalVariable *constant;
	llvm::Constant *array_struct;
	std::vector<llvm::Constant *> array_struct_fields;
	std::vector<llvm::Constant *> tmp_struct;

	for(int i = 0; i < p->sizelocvars; i++) {
		LocVar* locvar = &(p->locvars[i]);
		tmp_struct.clear();
		tmp_struct.push_back(GetGlobalStr(context, module, getstr(locvar->varname)));
		tmp_struct.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, locvar->startpc)));
		tmp_struct.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, locvar->endpc)));
		array_struct_fields.push_back(llvm::ConstantStruct::get(vm.t_jit_LocVar, tmp_struct));
	}

	array_struct = llvm::ConstantStruct::getAnon(context, array_struct_fields, false);
	constant = new llvm::GlobalVariable(*module, array_struct->getType(), true,
		llvm::GlobalValue::InternalLinkage, array_struct, ".locvars");

	return constant;
}

llvm::GlobalVariable *LLVMDumper::BuildUpValues(llvm::LLVMContext& context, llvm::Module* module, Proto *p) {
	llvm::GlobalVariable *constant;
	llvm::Constant *array_struct;
	std::vector<llvm::Constant *> array_struct_fields;

	for(int i = 0; i < p->sizeupvalues; i++) {
		array_struct_fields.push_back(GetGlobalStr(context, module, getstr(p->upvalues[i])));
	}

	array_struct = llvm::ConstantStruct::getAnon(context, array_struct_fields, false);
	constant = new llvm::GlobalVariable(*module, array_struct->getType(), true,
		llvm::GlobalValue::InternalLinkage, array_struct, ".upvalues");

	return constant;
}

llvm::Constant* LLVMDumper::BuildProtoGLobalVar(llvm::LLVMContext& context, llvm::Module* module, VMModule& vm, Proto *p)
{
	std::vector<llvm::Constant *> jit_proto_fields;
	llvm::GlobalVariable *tmp_global;
	llvm::Constant *tmp_constant;

	// name
	jit_proto_fields.push_back(GetGlobalStr(context, module, getstr(p->source)));

	auto func_name = compiler->GetFunctionName(p);
	auto function = module->getFunction(func_name);

	// jit_func
	if (function)
	{
		jit_proto_fields.push_back(function);
	} else
	{
		jit_proto_fields.push_back(llvm::Constant::getNullValue(vm.t_lua_func_ptr));
	}

	// linedefined
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(32,p->linedefined)));
	// lastlinedefined
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(32,p->lastlinedefined)));
	// nups
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(8,p->nups)));
	// numparams
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(8,p->numparams)));
	// is_vararg
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(8,p->is_vararg)));
	// maxstacksize
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(8,p->maxstacksize)));
	// sizek
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(16,p->sizek)));
	// sizelocvars
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(16,p->sizelocvars)));
	// sizeupvalues
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(32,p->sizeupvalues)));
	// sizep
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(32,p->sizep)));
	// sizecode
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(32,p->sizecode)));
	// sizelineinfo
	jit_proto_fields.push_back(llvm::ConstantInt::get(context, llvm::APInt(32,p->sizelineinfo)));
	// k
	jit_proto_fields.push_back(
		llvm::ConstantExpr::getCast(llvm::Instruction::BitCast, BuildConstants(context, module, vm, p), vm.t_constant_type_ptr));
	// locvars
	jit_proto_fields.push_back(
		llvm::ConstantExpr::getCast(llvm::Instruction::BitCast, BuildLocVars(context, module, vm, p), vm.t_jit_LocVar_ptr));
	// upvalues
	jit_proto_fields.push_back(
		llvm::ConstantExpr::getCast(llvm::Instruction::BitCast, BuildUpValues(context, module, p), llvm::PointerType::get(vm.t_str_ptr, 0)));

	// p
	if (p->sizep > 0)
	{
		std::vector<llvm::Constant *> tmp_array(p->sizep);
		for (int i = 0; i < p->sizep; i++)
		{
			tmp_array[i] = BuildProtoGLobalVar(context, module, vm, p->p[i]);
		}

		tmp_constant = llvm::ConstantArray::get(llvm::ArrayType::get(vm.t_jit_proto, p->sizep),tmp_array);

		tmp_global = new llvm::GlobalVariable(*module, tmp_constant->getType(), false,
			llvm::GlobalValue::InternalLinkage, tmp_constant, ".sub_protos");

		jit_proto_fields.push_back(GetConstPtr(context, tmp_global));
	} else {
		jit_proto_fields.push_back(llvm::Constant::getNullValue(vm.t_jit_proto_ptr));
	}


	// code
	if (p->sizecode > 0)
	{
		std::vector<llvm::Constant *> tmp_array;
		for (int i = 0; i < p->sizecode; i++)
		{
			tmp_array.push_back(llvm::ConstantInt::get(context, llvm::APInt(32,p->code[i])));
		}
		tmp_constant = llvm::ConstantArray::get(llvm::ArrayType::get(llvm::IntegerType::get(context, 32),p->sizecode),tmp_array);
		tmp_global = new llvm::GlobalVariable(*module, tmp_constant->getType(), false,
			llvm::GlobalValue::InternalLinkage, tmp_constant, ".proto_code");
		jit_proto_fields.push_back(GetConstPtr(context, tmp_global));
	} else {
		jit_proto_fields.push_back(llvm::Constant::getNullValue(llvm::PointerType::get(llvm::IntegerType::get(context, 32), 0)));
	}

	// lineinfo
	if (p->sizelineinfo > 0)
	{
		std::vector<llvm::Constant *> tmp_array;
		for (int i = 0; i < p->sizelineinfo; i++)
		{
			tmp_array.push_back(llvm::ConstantInt::get(context, llvm::APInt(32,p->lineinfo[i])));
		}
		tmp_constant = llvm::ConstantArray::get(llvm::ArrayType::get(llvm::IntegerType::get(context, 32),p->sizelineinfo),tmp_array);
		tmp_global = new llvm::GlobalVariable(*module, tmp_constant->getType(), false,
			llvm::GlobalValue::InternalLinkage, tmp_constant, ".proto_lineinfo");
		jit_proto_fields.push_back(GetConstPtr(context, tmp_global));
	} else {
		jit_proto_fields.push_back(llvm::Constant::getNullValue(llvm::PointerType::get(llvm::IntegerType::get(context, 32), 0)));
	}

	return llvm::ConstantStruct::get(vm.t_jit_proto, jit_proto_fields);
}

void LLVMDumper::BuildStandalone(llvm::LLVMContext& context, llvm::Module* module, VMModule& vm, Proto *p)
{
	//
	// dump protos to a global variable for re-loading.
	//
	auto jit_proto = BuildProtoGLobalVar(context, module, vm, p);

	auto gjit_proto_init = new llvm::GlobalVariable(*module, vm.t_jit_proto, false,
		llvm::GlobalValue::ExternalLinkage, jit_proto, "jit_proto_init");
}

std::string LLVMDumper::NormalizeModuleName(std::string mod_name)
{
	// remove '.bc' from end of mod_name.
	size_t n = mod_name.size()-3;
	if (n > 0)
	{
		std::string tmp = mod_name.substr(n, 3);
		if (tmp[0] == '.')
		{
			if (tmp[1] == 'b' || tmp[1] == 'B')
			{
				if (tmp[2] == 'c' || tmp[2] == 'C')
				{
					mod_name = mod_name.substr(0, n);
				}
			}
		}
	}
	// convert non-alphanum chars to '_'
	for (n = 0; n < mod_name.size(); n++)
	{
		char c = mod_name[n];
		if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) continue;
		if(c == '\n' || c == '\r') {
			mod_name = mod_name.substr(0,n);
			break;
		}
		mod_name[n] = '_';
	}

	return mod_name;
}


void LLVMDumper::BuildLuaModule(llvm::LLVMContext& context, llvm::Module* module, VMModule& vm, Proto *p, std::string mod_name)
{
	llvm::IRBuilder<> Builder(context);

	std::string name = "luaopen_" + NormalizeModuleName(mod_name);
	//
	// dump protos to a static variable for re-loading.
	//
	auto jit_proto = BuildProtoGLobalVar(context, module, vm, p);
	auto gjit_proto_init = new llvm::GlobalVariable(*module, vm.t_jit_proto, false,
		llvm::GlobalValue::InternalLinkage, jit_proto, "jit_proto_init");

	//
	// dump 'luaopen_<mod_name>' for loading the module.
	//
	auto func = llvm::Function::Create(vm.t_lua_func, llvm::Function::ExternalLinkage, name, module);
	// name arg1 = "L"
	auto func_L = func->arg_begin();
	func_L->setName("L");
	// entry block
	auto block = llvm::BasicBlock::Create(context, "entry", func);
	Builder.SetInsertPoint(block);

	// call 'load_compiled_module'
	auto load_compiled_module_func = module->getFunction("load_compiled_module");
	if (load_compiled_module_func == nullptr)
	{
		auto func_type = llvm::FunctionType::get(vm.t_int32, {func_L->getType(), vm.t_jit_proto_ptr}, false);
		load_compiled_module_func = llvm::Function::Create(func_type,
																											 llvm::Function::ExternalLinkage, "load_compiled_module", module);
	}

	auto call = Builder.CreateCall(load_compiled_module_func, {func_L, gjit_proto_init});
	call->setTailCall(true);
	Builder.CreateRet(call);

	//func->dump();
}

