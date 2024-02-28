//
// Created by Alex Zelenshikov on 27.02.2024.
//

#ifndef LLVM_LUA_VMMODULE_H
#define LLVM_LUA_VMMODULE_H

#include "lua_vm_ops.h"
#include "lopcodes.h"

struct OPFunctionVariant
{
  const vm_func_info* info;
  llvm::Function* func = nullptr;

  OPFunctionVariant(const vm_func_info* info) : info(info) {}
};

struct OPFunction {
public:
  std::unordered_map<hint_t, std::unique_ptr<OPFunctionVariant>> variants;
};

class VMModule
{
public:
  llvm::Type* t_init32;
  llvm::Type* t_void;
  llvm::Type* t_int64;
  llvm::Type* t_double;

  llvm::Type* t_TValue;
  llvm::Type* t_LClosure;
  llvm::Type* t_lua_State;

  llvm::Type* t_TValue_ptr;
  llvm::Type* t_LClosure_ptr;
  llvm::Type* t_lua_State_ptr;

  llvm::FunctionType* t_lua_func;

  VMModule();

  llvm::orc::ThreadSafeModule Load(llvm::orc::ThreadSafeContext context);

  llvm::Function* func(const char* name) { return functions[name]; }

  OPFunctionVariant* op_func(int opcode, hint_t hint);

private:
  std::unique_ptr<llvm::Module> vm_module;

  std::unordered_map<const char*, llvm::Function*> functions;
  OPFunction op_functions[NUM_OPCODES];

  void CollectVMTypes(llvm::LLVMContext& context);
  void CollectVMFunctions(llvm::Module* module);
  void CollectVMOpcodes(llvm::LLVMContext& context, llvm::Module* module);

  void FindOrCreateFunction(llvm::Module* module, llvm::Type *result, llvm::ArrayRef<llvm::Type*> params, const char* name);
  llvm::Type* GetVarType(llvm::LLVMContext& context, val_t type, hint_t hints);
};


#endif //LLVM_LUA_VMMODULE_H
