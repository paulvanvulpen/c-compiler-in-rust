pub fn generate_unique_id() -> usize {
    static TMP_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    TMP_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

pub fn make_temporary_from_identifier(name: &str) -> String {
    let id = generate_unique_id();
    format!("{name}.{id}")
}

pub fn make_label(label: &str) -> String {
    static LABEL_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let id = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    format!("{label}{id}")
}
