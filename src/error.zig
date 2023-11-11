pub const CompileError = error{
    tooManyConstants,
};

pub const RuntimeError = error{
    UndefinedGlobal,
};
