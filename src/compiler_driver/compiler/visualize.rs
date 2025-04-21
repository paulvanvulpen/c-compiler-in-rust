pub trait Visualizer {
    fn visualize(&self, depth: u8) -> String;
}
