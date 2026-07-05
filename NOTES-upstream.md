# NOTES: box-nd work to unblock / streamline apecs-box2d & apecs-box3d

Companion to the 2026-07-06 triage in `TODO-boxes.md`: everything the
apecs wrappers want from `../box-nd/` (packages `Box2D`, `Box3D`), in
priority order. The recurring theme is one gap: several C structs are
bound as **opaque tags** (`Box2D.Tags`/`Box3D.Tags` — no `Storable`, no
constructor, no size), which makes every binding that takes or returns a
pointer to them uncallable from Haskell. Most items below are "promote a
tag to a peekable/pokeable struct in `Types.hsc`" plus, where a struct
holds pointers into caller memory, a hand-written `with…` bracket that
owns the borrow (the `Callbacks.hs` pattern). The generated modules say
"edit the generator, not this file" — so these are `boxnd-gen` and/or
`Types.hsc`/hand-module changes, not patches to generated output.

## Unblockers (apecs features are waiting on these)

### 1. `ShapeProxy` construction — unblocks `overlapShape` + `castShape`

`b2World_OverlapShape`/`b2World_CastShape` (and b3) are bound but take
`Ptr ShapeProxy`, an opaque tag. The structs differ per package:

- 2D `b2ShapeProxy`: inline `points[B2_MAX_POLYGON_VERTICES]` + `count`
  + `radius` — a plain fixed-size struct; a `Storable` in `Types.hsc` is
  enough, and the already-bound `Collision.makeProxy` becomes usable via
  `alloca`.
- 3D `b3ShapeProxy`: `{ const b3Vec3* points; int count; float radius }`
  — points into caller memory, so it wants a bracket that keeps the
  point array alive:
  `withShapeProxy :: VS.Vector Vec3 -> Float -> (Ptr ShapeProxy -> IO a) -> IO a`.

Suggested surface: the `withShapeProxy` bracket in *both* packages (2D
can build it on the Storable) so the apecs code is symmetric.

### 2. `ContactData`/`Manifold` peekable — unblocks manifold data on `Collisions`

`ContactBeginTouchEvent` carries `shapeIdA/B` + `contactId`;
`Contact.getData` is bound but writes into `Ptr ContactData` (opaque).
The C structs are plain data:

- `b2ContactData { contactId, shapeIdA, shapeIdB, manifold }`
- `b2Manifold { normal, rollingImpulse, points[2], count, ... }` plus
  `b2ManifoldPoint` (3D: `b3Manifold`, more points — check the header).

Work: `Storable` instances for `ManifoldPoint`, `Manifold`,
`ContactData` in both `Types.hsc`, and regenerate `Contact.getData` as
`ContactId -> IO ContactData` (the `allocaOut` idiom). apecs then
resolves the contact id per begin-touch event and exposes point/normal
on `Collision` values.

### 3. `MeshDef`/`HeightFieldDef` construction — unblocks user-authored 3D level geometry

Today `b3CreateMesh`/`b3CreateHeightField` are bound but their defs are
opaque, so the only reachable mesh/height-field data is the procedural
generators (box, grid, torus, wave, ...) — real levels from model files
cannot be loaded. The defs are pointer-carrying structs:

- `b3MeshDef { b3Vec3* vertices; int32_t* indices; uint8_t*
  materialIndices; weldTolerance; vertexCount; triangleCount;
  weldVertices; ... }`
- `b3HeightFieldDef { float* heights; uint8_t* materialIndices
  (0xFF = hole); b3Vec3 scale; countX; countZ; min/max heights ... }`

Suggested surface: skip exposing the raw defs and provide convenience
creators in hand modules, mirroring the generator functions' shape:

```haskell
createMeshFromData :: VS.Vector Vec3 -> VS.Vector Int32
                   -> Maybe (VS.Vector Word8) -> MeshOptions
                   -> IO (Ptr MeshData)   -- also surface the
                                          -- degenerate-triangle out-param
createHeightFieldFromData :: VS.Vector Float -> Maybe (VS.Vector Word8)
                          -> Vec3 -> Int -> Int -> IO (Ptr HeightFieldData)
```

`b3CreateMesh`/`b3CreateHeightField` clone the def contents, so
temporary `unsafeWith` borrows suffice (verify against `mesh.c`/
`height_field.c` when implementing). apecs then feeds the result
straight into its existing GC-managed `Mesh`/`HeightField` handles —
no apecs-side changes beyond re-exporting the creators.

### 4. `CompoundDef` + child defs — unblocks 3D `GeoCompound`

`b3CompoundDef` holds arrays of `b3CompoundSphereDef`/`CapsuleDef`/
`HullDef`/`MeshDef` children; all are opaque tags. Needs the child def
`Storable`s plus either a `Storable CompoundDef` with a
`withCompoundDef` bracket (arrays borrowed from vectors) or a builder
shim. `b3CreateCompound` clones all input data (its haddock says so), so
borrows are enough. Do this after §3 — it reuses the same hull/mesh data
handling. apecs side afterwards: a `GeoCompound` constructor over the
existing GC-handle pattern.

### 5. `hsg_b3Shape_TestPoint` shim — deletes apecs' analytic containment

2D has `b2Shape_TestPoint`; 3D has no equivalent, and
`b3Shape_GetClosestPoint`'s `useRadii` mode keeps the witness point on
the perimeter even when the target is inside, which forced
`containsPointQuery` to special-case spheres and capsules analytically.
Shim: run `b3ShapeDistance` with `useRadii` and return
`distance == 0` — the *distance* field is clamped correctly; only the
witness point is pushed out. One small C function + generated binding
`Shape.testPoint :: ShapeId -> Vec3 -> IO Bool`, mirroring 2D. apecs
then replaces the whole per-shape dispatch with the engine's answer.

### 6. `CollisionPlane`/`PlaneSolverResult` binding — deletes the hand-ported mover solver

`Collision.solvePlanes`/`clipVector` are bound but uncallable (both
types opaque), so both apecs wrappers carry verbatim Haskell ports of
`b2SolvePlanes`/`b2ClipVector` (~80 lines each) that can silently drift
from upstream `mover.c`. The structs are tiny:

- `b2CollisionPlane { b2Plane plane; float pushLimit; float push;
  bool clipVelocity }` (`Plane` is already Storable)
- `b2PlaneSolverResult { b2Vec2 translation; int iterationCount }`

Work: `Storable` for both in each `Types.hsc`, then a friendly surface —
note the C function mutates the planes' `push` accumulators in place and
`clipVector` reads them back, so the natural Haskell shape is:

```haskell
solvePlanes :: Vec2 -> VS.Vector CollisionPlane
            -> IO (Vec2, VS.Vector CollisionPlane, Int)
clipVector  :: Vec2 -> VS.Vector CollisionPlane -> IO Vec2
```

(thaw/borrow internally). apecs then deletes `solveMoverPlanes`/
`solvePlanesStep`/`clipMoverVector` + the vec helpers in both packages;
`moveCharacter` keeps its loop but calls the engine. This also retires
the hard-coded `linearSlop = 0.005` assumption (see §8).

## Streamliners (smaller wins, do opportunistically)

### 7. Chain-level contact/hit event enabling (2D)

`b2CreateChain` hard-codes segment `enableContactEvents`/
`enableHitEvents` off; the apecs chain-resolution work has to
`getSegments` and toggle each segment after create. A
`hsg_b2Chain_EnableContactEvents`-style shim looping the segments in C
(or an upstream box2d PR adding the flags to `b2ChainDef`) turns N FFI
calls into one. Not blocking — apecs can ship the per-segment loop.

### 8. Bind `b2GetLengthUnitsPerMeter` / b3 equivalent

`B2_LINEAR_SLOP` is `0.005 * lengthUnitsPerMeter`; the apecs mover port
assumes the default 1.0 because the getter isn't bound. Trivial binding;
becomes moot for the mover once §6 lands, but the constant leaks into
other tuning docs too (sleep thresholds, tolerances), so it's worth
having regardless.

### 9. [engine-level] 3D world snapshot/restore parity

2D has `b2World_Snapshot`/`Restore`/`CreateFromSnapshot` (wrapped as
`snapshotWorld`/`restoreWorld` in apecs-box2d); Box3D has no equivalent
at all, so apecs-box3d cannot offer rewind. This is a box3d engine
feature, not a binding gap — noting it here so the asymmetry is tracked
where it can actually be fixed.

## Ports to delete from apecs once the above land

- §5 → the sphere/capsule analytic dispatch in 3D `containsPointQuery`.
- §6 → `solveMoverPlanes`/`solvePlanesStep`/`clipMoverVector`, their vec
  helpers and `linearSlop`/`solverIterations` constants, in *both*
  wrappers (keep the collide→solve→cast loop and its constants).
- §7 → nothing to delete, but the chain-resolution item shrinks to
  registry work only.
