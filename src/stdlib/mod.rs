pub mod math;
pub mod io;
pub mod string;
pub mod time;

use crate::native_interface::NativeRegistry;

pub fn register_stdlib(registry: &mut NativeRegistry) {
    registry.register("abs", Box::new(math::AbsFunction));
    registry.register("max", Box::new(math::MaxFunction));
    registry.register("min", Box::new(math::MinFunction));
    registry.register("sqrt", Box::new(math::SqrtFunction));
    registry.register("pow", Box::new(math::PowFunction));
    registry.register("floor", Box::new(math::FloorFunction));
    registry.register("ceil", Box::new(math::CeilFunction));
    registry.register("round", Box::new(math::RoundFunction));
    
    registry.register("print", Box::new(io::PrintFunction));
    registry.register("println", Box::new(io::PrintlnFunction));
    registry.register("eprint", Box::new(io::EprintFunction));
    registry.register("eprintln", Box::new(io::EprintlnFunction));
    registry.register("read_line", Box::new(io::ReadLineFunction));
    
    registry.register("len", Box::new(string::LenFunction));
    registry.register("to_upper", Box::new(string::ToUpperFunction));
    registry.register("to_lower", Box::new(string::ToLowerFunction));
    registry.register("trim", Box::new(string::TrimFunction));
    registry.register("split", Box::new(string::SplitFunction));
    registry.register("join", Box::new(string::JoinFunction));
    registry.register("contains", Box::new(string::ContainsFunction));
    registry.register("starts_with", Box::new(string::StartsWithFunction));
    registry.register("ends_with", Box::new(string::EndsWithFunction));
    
    registry.register("now", Box::new(time::NowFunction));
    registry.register("now_millis", Box::new(time::NowMillisFunction));
    registry.register("now_micros", Box::new(time::NowMicrosFunction));
    registry.register("sleep", Box::new(time::SleepFunction));
    registry.register("sleep_millis", Box::new(time::SleepMillisFunction));
    registry.register("timer_start", Box::new(time::TimerStartFunction));
    registry.register("timer_elapsed", Box::new(time::TimerElapsedFunction));
}