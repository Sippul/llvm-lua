//
// Created by Alex Zelenshikov on 27.02.2024.
//

#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>

#include "VMModule.h"
#include "load_embedded_bc.h"
#include "lua_vm_ops_bc.h"

VMModule::VMModule()
{

}

llvm::orc::ThreadSafeModule VMModule::Load(llvm::orc::ThreadSafeContext ts_context)
{
  llvm::LLVMContext& context = *ts_context.getContext();
  auto module = LoadEmbeddedBitcode(context, "lua_vm_ops_bc", lua_vm_ops_bc, sizeof(lua_vm_ops_bc));

  CollectVMTypes(context);
  CollectVMFunctions(module.get());
  CollectVMOpcodes(context, module.get());

  return {std::move(module), ts_context};
}

void VMModule::CollectVMTypes(llvm::LLVMContext& context)
{
  t_init32 = llvm::Type::getInt32Ty(context);
  t_void = llvm::Type::getVoidTy(context);

  t_int64 = llvm::Type::getInt64Ty(context);
  t_double = llvm::Type::getDoubleTy(context);

  t_TValue = llvm::StructType::getTypeByName(context, "struct.lua_TValue");
  if (t_TValue == nullptr)
  {
    t_TValue = llvm::StructType::getTypeByName(context, "struct.TValue");
  }
  t_LClosure = llvm::StructType::getTypeByName(context, "struct.LClosure");
  t_lua_State = llvm::StructType::getTypeByName(context, "struct.lua_State");

  t_TValue_ptr = llvm::PointerType::getUnqual(t_TValue);
  t_LClosure_ptr = llvm::PointerType::getUnqual(t_LClosure);
  t_lua_State_ptr = llvm::PointerType::getUnqual(t_lua_State);

  t_lua_func = llvm::FunctionType::get(t_init32, {t_lua_State_ptr}, false);
}

void VMModule::CollectVMFunctions(llvm::Module* module)
{
  FindOrCreateFunction(module, t_void, {t_lua_State_ptr, t_LClosure_ptr, t_init32}, "vm_next_OP");
  FindOrCreateFunction(module, t_void, {t_lua_State_ptr, t_LClosure_ptr, t_init32, t_init32}, "vm_print_OP");
  FindOrCreateFunction(module, t_void, {t_init32}, "vm_count_OP");
  FindOrCreateFunction(module, t_void, {t_lua_State_ptr, t_LClosure_ptr, t_init32, t_init32}, "vm_mini_vm");

  auto funcs_to_find = {
      "vm_get_current_closure",
      "vm_get_current_constants",
      "vm_get_number",
      "vm_get_long"
      "vm_set_number",
      "vm_set_long"
  };

  for (auto name : funcs_to_find)
  {
    functions[name] = module->getFunction(name);
  }
}

void VMModule::FindOrCreateFunction(llvm::Module* module, llvm::Type *result, llvm::ArrayRef<llvm::Type*> params, const char* name)
{
  functions[name] = module->getFunction(name);
  if (functions[name] == nullptr)
  {
    auto func_type = llvm::FunctionType::get(result, params, false);
    functions[name] = llvm::Function::Create(func_type,
                                        llvm::Function::ExternalLinkage, name, module);
  }
}

void VMModule::CollectVMOpcodes(llvm::LLVMContext& context, llvm::Module* module)
{
  for (int i = 0; true; ++i)
  {
    const vm_func_info* func_info = &vm_op_functions[i];
    auto opcode = func_info->opcode;
    if (opcode < 0) {
      break;
    }

    auto op_function = std::make_unique<OPFunctionVariant>(func_info);

    llvm::Function* func = module->getFunction(func_info->name);
    if (func != NULL)
    {
      op_function->func = func;
    }
    else
    {
      std::vector<llvm::Type*> func_args;

      for(int x = 0; func_info->params[x] != VAR_T_VOID; x++) {
        func_args.push_back(GetVarType(context, func_info->params[x], func_info->hint));
      }

      auto func_type = llvm::FunctionType::get(
          GetVarType(context, func_info->ret_type, func_info->hint), func_args, false);
      func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
                                    func_info->name, module);
      op_function->func = func;
    }

    op_functions[opcode].variants[func_info->hint] = std::move(op_function);
  }
}

llvm::Type* VMModule::GetVarType(llvm::LLVMContext& context, val_t type, hint_t hints)
{
  switch(type) {
    case VAR_T_VOID:
      return llvm::Type::getVoidTy(context);
    case VAR_T_INT:
    case VAR_T_ARG_A:
    case VAR_T_ARG_B:
    case VAR_T_ARG_BK:
    case VAR_T_ARG_Bx:
    case VAR_T_ARG_Bx_NUM_CONSTANT:
    case VAR_T_ARG_B_FB2INT:
    case VAR_T_ARG_sBx:
    case VAR_T_ARG_C:
    case VAR_T_ARG_CK:
    case VAR_T_ARG_C_NUM_CONSTANT:
    case VAR_T_ARG_C_NEXT_INSTRUCTION:
    case VAR_T_ARG_C_FB2INT:
    case VAR_T_PC_OFFSET:
    case VAR_T_INSTRUCTION:
    case VAR_T_NEXT_INSTRUCTION:
      return llvm::Type::getInt32Ty(context);
    case VAR_T_LUA_STATE_PTR:
      return t_lua_State_ptr;
    case VAR_T_K:
      return t_TValue_ptr;
    case VAR_T_CL:
      return t_LClosure_ptr;
    case VAR_T_OP_VALUE_0:
    case VAR_T_OP_VALUE_1:
    case VAR_T_OP_VALUE_2:
      if(hints & HINT_USE_LONG) {
        return llvm::Type::getInt64Ty(context);
      }
      return llvm::Type::getDoubleTy(context);
    default:
      fprintf(stderr, "Error: missing var_type=%d\n", type);
      exit(1);
  }
}

OPFunctionVariant* VMModule::op_func(int opcode, hint_t hint)
{
  return op_functions[opcode].variants[hint].get();
}



//llvm::Function* VMModule::CreateFunction()
//{
//
//}