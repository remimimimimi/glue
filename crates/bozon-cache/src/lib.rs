//! An implementation of a cache system with filesystem drain.
// !
// ! # Features
// !
// ! * Optional different cache limit tactics.

use std::{
    borrow::Borrow,
    hash::{BuildHasher, Hash},
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(feature = "hashbrown")]
use hashbrown::HashMap;
#[cfg(not(feature = "hashbrown"))]
use std::collections::HashMap;

#[cfg(feature = "hashbrown")]
use hashbrown::hash_map::Iter;
#[cfg(not(feature = "hashbrown"))]
use std::collections::hash_map::Iter;

#[cfg(feature = "hashbrown")]
use hashbrown::hash_map::IterMut;
#[cfg(not(feature = "hashbrown"))]
use std::collections::hash_map::IterMut;

#[cfg(feature = "hashbrown")]
pub type DefaultHasher = hashbrown::hash_map::DefaultHashBuilder;
#[cfg(not(feature = "hashbrown"))]
pub type DefaultHasher = std::collections::hash_map::RandomState;

/// Create a `Cache` from a list of key-value pairs
///
/// # Example
///
/// ```
/// let cache = bozon_cache::cache!{
///     1 => "a",
///     2 => "b",
///     3 => "c",
/// };
///
/// assert_eq!(cache.get(&1), Some(&"a"));
/// assert_eq!(cache.get(&2), Some(&"b"));
/// assert_eq!(cache.get(&3), Some(&"c"));
/// ```
#[macro_export(local_inner_macros)]
macro_rules! cache {
    ($($key:expr => $value:expr,)+) => { cache!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let mut _cache = ::bozon_cache::Cache::new();
            $(
                _cache.put($key, $value);
            )*
            _cache
        }
    };
}

/// Newtype over `HashMap` that provides different convinitet features.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Cache<K, V, S = DefaultHasher>
where
    K: Hash + Eq,
    S: BuildHasher + Default,
{
    map: HashMap<K, V, S>,
}

impl<K: Hash + Eq, V> Cache<K, V> {
    /// Creates a new `Cache`
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::Cache;
    ///
    /// let mut cache: Cache<String, u32> = Cache::new();
    /// ```
    pub fn new() -> Cache<K, V> {
        Cache {
            map: HashMap::new(),
        }
    }
}

impl<K: Hash + Eq, V, S: BuildHasher + Default> Cache<K, V, S> {
    /// Creates a new `Cache` that uses the provided hash builder.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::{Cache, DefaultHasher};
    ///
    /// let hasher = DefaultHasher::default();
    /// let mut cache: Cache<String, u32> = Cache::with_hasher(hasher);
    /// ```
    pub fn with_hasher(hash_builder: S) -> Cache<K, V, S> {
        Cache {
            map: HashMap::with_hasher(hash_builder),
        }
    }

    /// Inserts a key-value pair into cache. If the key already exists in the cache, then it updates
    /// the key's value and returns the old value. Otherwise, `None` is returned.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::Cache;
    ///
    /// let mut cache = Cache::new();
    ///
    /// assert_eq!(cache.insert(1, "a"), None);
    /// assert_eq!(cache.insert(2, "b"), None);
    /// assert_eq!(cache.insert(2, "beta"), Some("b"));
    ///
    /// assert_eq!(cache.get(&1), Some(&"a"));
    /// assert_eq!(cache.get(&2), Some(&"beta"));
    /// ```
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.map.insert(k, v)
    }

    /// Same as `insert` but ignores previous value if there's one.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::Cache;
    ///
    /// let mut cache = Cache::new();
    ///
    /// cache.put(1, "a");
    /// cache.put(2, "b");
    /// cache.put(2, "beta");
    ///
    /// assert_eq!(cache.get(&1), Some(&"a"));
    /// assert_eq!(cache.get(&2), Some(&"beta"));
    /// ```
    pub fn put(&mut self, k: K, v: V) {
        self.insert(k, v);
    }

    /// Returns a reference to the value of the key in the cache or `None` if it is not
    /// present in the cache.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::Cache;
    ///
    /// let mut cache = Cache::new();
    ///
    /// cache.put(1, "a");
    /// cache.put(2, "b");
    /// cache.put(2, "c");
    /// cache.put(3, "d");
    ///
    /// assert_eq!(cache.get(&1), Some(&"a"));
    /// assert_eq!(cache.get(&2), Some(&"c"));
    /// assert_eq!(cache.get(&3), Some(&"d"));
    /// ```
    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(k)
    }

    /// Returns a mutable reference to the value of the key in the cache or `None` if it
    /// is not present in the cache.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::Cache;
    ///
    /// let mut cache = Cache::new();
    ///
    /// cache.put(1, "a");
    /// cache.put(2, "b");
    /// cache.put(2, "c");
    /// cache.put(3, "d");
    ///
    /// assert_eq!(cache.get(&1), Some(&"a"));
    /// assert_eq!(cache.get(&2), Some(&"c"));
    /// assert_eq!(cache.get(&3), Some(&"d"));
    ///
    /// *cache.get_mut(&2).unwrap() = "b";
    /// *cache.get_mut(&3).unwrap() = "c";
    ///
    /// assert_eq!(cache.get(&1), Some(&"a"));
    /// assert_eq!(cache.get(&2), Some(&"b"));
    /// assert_eq!(cache.get(&3), Some(&"c"));
    /// ```
    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get_mut(k)
    }

    /// Returns a bool indicating whether the given key is in the cache.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::Cache;
    ///
    /// let mut cache = Cache::new();
    ///
    /// cache.put(2, "b");
    /// cache.put(3, "c");
    ///
    /// assert!(!cache.contains_key(&1));
    /// assert!(cache.contains_key(&2));
    /// assert!(cache.contains_key(&3));
    /// ```
    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.contains_key(k)
    }

    /// Returns the number of key-value pairs that are currently in the cache.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::Cache;
    ///
    /// let mut cache = Cache::new();
    /// assert_eq!(cache.len(), 0);
    ///
    /// cache.put(1, "a");
    /// assert_eq!(cache.len(), 1);
    ///
    /// cache.put(2, "b");
    /// assert_eq!(cache.len(), 2);
    ///
    /// cache.put(2, "c");
    /// assert_eq!(cache.len(), 2);
    /// ```
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Returns a bool indicating whether the cache is empty or not.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::Cache;
    ///
    /// let mut cache = Cache::new();
    /// assert!(cache.is_empty());
    ///
    /// cache.put(1, "a");
    /// assert!(!cache.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Clears the cache, removing all key-value pairs. Keeps the allocated memory
    /// for reuse.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::Cache;
    ///
    /// let mut cache = Cache::new();
    ///
    /// cache.put(1, "a");
    /// cache.clear();
    ///
    /// assert!(cache.is_empty());
    /// ```
    pub fn clear(&mut self) {
        self.map.clear()
    }

    /// An iterator visiting all key-value pairs in arbitrary order.
    /// The iterator element type if `(&'a K, &'a V)`.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::cache;
    ///
    /// let mut cache = cache! {
    ///     1 => "a",
    ///     2 => "b",
    ///     3 => "c",
    /// };
    ///
    /// for (key, value) in cache.iter() {
    ///     dbg!(key, value);
    /// }
    /// ```
    pub fn iter(&self) -> Iter<'_, K, V> {
        self.map.iter()
    }

    /// An iterator visiting all key-value pairs in arbitrary order.
    /// The iterator element type if `(&'a K, &'a V)`.
    ///
    /// # Example
    ///
    /// ```
    /// use bozon_cache::cache;
    ///
    /// let mut cache = cache! {
    ///     "a" => 0,
    ///     "b" => 1,
    ///     "c" => 2,
    /// };
    ///
    /// for (key, value) in cache.iter_mut() {
    ///     *value = *value + 1;
    /// }
    ///
    /// assert_eq!(cache.get(&"a"), Some(&1));
    /// assert_eq!(cache.get(&"b"), Some(&2));
    /// assert_eq!(cache.get(&"c"), Some(&3));
    /// ```
    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
        self.map.iter_mut()
    }
}

impl<'a, K: Hash + Eq, V, S: BuildHasher + Default> IntoIterator for &'a Cache<K, V, S> {
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    fn into_iter(self) -> Iter<'a, K, V> {
        self.map.iter()
    }
}

impl<'a, K: Hash + Eq, V, S: BuildHasher + Default> IntoIterator for &'a mut Cache<K, V, S> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;

    fn into_iter(self) -> IterMut<'a, K, V> {
        self.map.iter_mut()
    }
}
