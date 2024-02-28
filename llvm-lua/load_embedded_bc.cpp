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

#include <stdlib.h>
#include <stdio.h>

#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"

#include <string>

#include "load_embedded_bc.h"

std::string GetErrorMessage(llvm::Error error)
{
  std::string error_msg;
  handleAllErrors(std::move(error), [&](llvm::ErrorInfoBase &EIB) {
    error_msg = EIB.message();
  });

  return error_msg;
}

std::unique_ptr<llvm::Module> LoadEmbeddedBitcode(llvm::LLVMContext &context,
	const char *name, const unsigned char *start, size_t len)
{
	llvm::StringRef mem_ref((const char *)start, len - 1);

	auto buffer = llvm::MemoryBuffer::getMemBuffer(mem_ref, name);
	if (buffer)
  {
    if (auto module = llvm::getOwningLazyBitcodeModule(std::move(buffer), context))
    {
      if (auto error = module.get()->materializeAll())
      {
        printf("Failed to materialize embedded '%s' file: %s\n", name, GetErrorMessage(std::move(error)).c_str());
        exit(1);
      }

      return std::move(module.get());
    }
    else
    {
      printf("Failed to parse embedded '%s' file: %s\n", name, GetErrorMessage(module.takeError()).c_str());
      exit(1);
    }
  }

	return nullptr;
}

