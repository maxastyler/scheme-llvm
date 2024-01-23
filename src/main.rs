use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    types::{BasicType, FunctionType},
    values::FloatMathValue,
    OptimizationLevel,
};
use parser::Expression;

mod parser;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: inkwell::module::Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn gen_expression(&self, expression: Expression) {
        match expression {
            Expression::Define { identifier, value } => todo!(),
            Expression::Let { pairs, body } => todo!(),
            Expression::Lambda { arguments, body } => todo!(),
            Expression::Begin(_) => todo!(),
            Expression::List(_) => todo!(),
            Expression::Atom(a) => match a {
                parser::Atom::Number(n) => {
                    let fn_type = self.context.f64_type().fn_type(&[], false);
                    let fn_val = self.module.add_function("f", fn_type, None);

                    let block = self.context.append_basic_block(fn_val, "entry");
                    self.builder.position_at_end(block);
                    let f = self.context.f64_type().const_float(n);
                    self.builder.build_return(Some(&f));
                }
                parser::Atom::Identifier(_) => todo!(),
                parser::Atom::Nil => todo!(),
            },
        }
    }
}

type f_func = unsafe extern "C" fn() -> f64;

fn main() {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
    };

    codegen.gen_expression(10.0.into());
    println!("{:?}", unsafe {
        codegen
            .execution_engine
            .get_function::<f_func>("f")
            .ok()
            .unwrap()
            .call()
    });
}
