mod instruction;
mod program;
mod constant;
mod function;
mod debug;
mod types;

pub use instruction::Instruction;
pub use program::BytecodeProgram;
pub use constant::Constant;
pub use function::Function;
pub use types::InstructionPointer;

pub trait Compilable {
    fn compile(&self, program: &mut BytecodeProgram) -> Result<(), String>;
}