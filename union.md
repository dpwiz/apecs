# Research: Iterating over a Union of Components

In `apecs`, iterating over an intersection of components is well-supported (e.g. `cmap \(a :: A, b :: B) -> ...`).
This document explores alternative approaches for iterating over the *union* of components, i.e., retrieving entities that have component A, component B, or both.

## Requirements & Scope
- We primarily want to retrieve Entity IDs.
- Retrieving component values could use `Either A B`, `These A B` (from `these` package) or `(Maybe A, Maybe B)`.
- We need to avoid breaking existing functionality.
- Performance is a secondary concern but we want to avoid obvious pitfalls and flag any highly performant solutions.

## Alternatives

### 1. User-Space Composition
Use existing `apecs` functionality (e.g. `cmap`, `cfold`) to manually collect components or entity IDs.
- **Approach:** We could iterate over `A` and `B` separately and combine the results, potentially using `Not` to avoid duplicates (e.g. iterate over `A`, then iterate over `B` with `Not A`).
- **Pros:** Needs zero changes to the `apecs` core. Fully implemented in user space.
- **Cons:** Might be cumbersome for the user. Performance may be suboptimal as we iterate over the world multiple times.

### 2. Pseudo-component / Custom Store Instance
Create a new pseudocomponent, `Union a b` or implement `ExplMembers` for `Either a b` or `(Maybe a, Maybe b)`.
- **Approach:**
  - `Either a b` already has a component instance, but lacks an `ExplMembers` instance. We could try to provide one by combining the member lists.
  - Or, add a new `Union a b` that specifically returns `(Maybe a, Maybe b)` or just acts as a filter.
- **Pros:** Feels like native `apecs` functionality. Ergonimc for the user.
- **Cons:** Implementing `ExplMembers` for two stores requires merging two unboxed integer vectors. Doing this efficiently (and deduplicating them) might be tricky. The standard unboxed vector functions do not have an out-of-the-box `union` or `merge` that handles duplicates easily.

### 3. Core Store modifications
- **Approach:** Introduce a `union` primitive or special typeclasses into `Apecs.Core` to allow stores to define custom union behavior.
- **Pros:** Could yield the best performance if stores can optimize union iterations.
- **Cons:** High risk of breaking changes or polluting the core API.

## Proofs of Concept

*(To be filled in as PoCs are developed)*

### Proof of Concept 1: User-Space Composition
**Status:** Working

Using standard `apecs` functionality, it is possible to iterate over a union of `A` and `B` without defining any new components or stores. The approach is to do two passes:
1. `collect $ \(_ :: A, e :: Entity) -> Just e`
2. `collect $ \(_ :: B, _ :: Not A, e :: Entity) -> Just e`

Concatenating these two lists gives the full set of entities containing A, B, or both, exactly once. From there, one can fetch `(Maybe A, Maybe B)` using the normal `get` function.

- **Pros:** Does not require modifying `apecs` or introducing new typeclasses. The type signatures are clear.
- **Cons:**
  - Requires two passes over the ECS stores (`A`, and `B` with `Not A`), allocating intermediate lists or vectors.
  - Doing an n-ary union (e.g., A, B, or C) results in an exponential blow-up of `Not` queries if done manually.
  - Less ergonomic compared to `cmap`.
  - Fetching the entity and then separately calling `get` might incur additional hash lookups compared to doing everything in one batched vectorized operation.

### Proof of Concept 2: Pseudo-component / Custom Store Instance
**Status:** Working

By adding an `ExplMembers` instance to `EitherStore`, we can directly iterate over the union of components. `Either A B` behaves by yielding `B` if present, else `A` (if we just ask for values), but the critical part is that it yields the union of Entity IDs.
We implemented a naive merge of the two `explMembers` outputs:
```haskell
instance (ExplMembers m sa, ExplMembers m sb) => ExplMembers m (EitherStore sa sb) where
  explMembers (EitherStore sa sb) = do
    ma <- explMembers sa
    mb <- explMembers sb
    let listA = U.toList ma
        listB = U.toList mb
    return $ U.fromList $ L.nub $ L.sort $ listA ++ listB
```
This enables native `cmapM_ $ \(_ :: Either A B, e :: Entity) -> ...` usage which yields exactly the union.

- **Pros:**
  - Extremely ergonomic API (`Either A B` is built-in).
  - Can fetch values cleanly using `Either A B` or fetch a tuple of `(Maybe A, Maybe B)` in the loop body.
- **Cons:**
  - `EitherStore` does not have this instance currently in `apecs` core. Adding it is technically an orphan instance if done in user-space, but could be added to `apecs` core easily.
  - Performance: doing a naive list-based sort/nub is very slow. A properly optimized unboxed vector merge is necessary to make this competitive.
  - The `Either A B` values mask whether both components are present unless you explicitly re-query.

### Proof of Concept 3: Advanced Core Integration
**Status:** Working & High Performance

In this approach, we create a new `Union a b` pseudo-component and its corresponding `UnionStore`, which behaves natively in `apecs`. The `explMembers` function implements a true linear-time O(N+M) merge algorithm for Unboxed Vectors using `Data.Vector.Unboxed.Mutable`.
Since `Map` stores inherently return sorted keys (due to `IntMap.keys`), this merge immediately works flawlessly without sorting in intermediate steps.

```haskell
mergeSortedVectors :: U.Vector Int -> U.Vector Int -> U.Vector Int
mergeSortedVectors v1 v2 = U.create $ do
  -- ... standard O(N+M) two-pointer merge into an unboxed mutable vector
```
- **Pros:**
  - Incredible performance: avoids allocating lists, avoids O(N log N) sorting, and processes elements in exactly O(N+M) unboxed iterations.
  - Gives the best API: directly fetches `(Maybe a, Maybe b)` inline without having to manually query `get` again.
  - Clean and composable.
- **Cons:**
  - Requires vectors to be sorted. `apecs` stores like `Map` naturally do this. `Cache` does *not* necessarily return strictly sorted keys natively if it just iterates over a raw array. If `Cache` is used, the vectors must be sorted using `vector-algorithms` (intro sort) before merging, adding O(N log N) overhead per component vector.

## Conclusion & Recommendations

The user-space method (PoC 1) requires no core modifications, but creates too much overhead (O(N) unboxed lists allocating into standard lists, iterating them separately, and then manually `get`ting elements). It also scales extremely poorly for 3+ elements.

The custom instance approach (PoC 2) via `Either A B` gives a very smooth and native-feeling API. However, `Either A B` limits you to fetching the "first" matched element, obfuscating if both exist simultaneously. It also lacks a sorted/merged implementation currently in `apecs`.

The **Advanced Core Integration (PoC 3)** stands out as the winner in terms of API design, ergonomics, and raw performance. By defining a `Union a b` pseudo-component which explicitly yields `(Maybe a, Maybe b)` and uses a strict O(N+M) two-pointer merge on the unboxed vectors, `apecs` users can natively traverse disjoint or intersecting entities without additional hash lookups in the loop body.

**Recommendation:** Add a `Union a b` pseudo-component into `Apecs.Components` (or `EitherStore` instance if `(Maybe A, Maybe B)` is less preferred). Incorporate the O(N+M) unboxed vector merge function. For stores that do not guarantee sorted elements (e.g., `Cache`), the implementation could branch to use `vector-algorithms` intro-sort before merging. This balances robust API safety with the absolute minimum number of heap allocations.
