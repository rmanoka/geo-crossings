use geo::{GeoNum, LineString};

pub struct MonoPoly<T: GeoNum> {
    top: LineString<T>,
    bot: LineString<T>,
}

impl<T: GeoNum> MonoPoly<T> {
    pub fn new(top: LineString<T>, bot: LineString<T>) -> Self {
        assert_eq!(top.0.first(), bot.0.first());
        assert_eq!(top.0.last(), bot.0.last());
        assert_ne!(top.0.first(), top.0.last());
        Self { top, bot }
    }

    /// Get a reference to the mono poly's top.
    #[must_use]
    pub fn top(&self) -> &LineString<T> {
        &self.top
    }

    /// Get a reference to the mono poly's bot.
    #[must_use]
    pub fn bot(&self) -> &LineString<T> {
        &self.bot
    }

    pub fn into_ls_pair(self) -> (LineString<T>, LineString<T>) {
        (self.top, self.bot)
    }
}
