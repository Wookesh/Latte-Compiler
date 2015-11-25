module CompileLatte where


class Compile a where
	compile a -> StateT Store IO b