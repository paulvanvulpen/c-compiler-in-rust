pub fn generate_unique_id() -> usize {
    static TMP_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    TMP_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}
