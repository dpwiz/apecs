# NOTES: box-nd work to unblock / streamline apecs-box2d & apecs-box3d

Companion to the triage in `TODO-boxes.md`. Most of the original list
landed upstream in box-nd `33ecce4` ("Expose contact manifolds, mover
solvers and 3D geometry authoring") and has been consumed on the apecs
side (2026-07-06); this file now tracks the remainder, with the landed
items kept below for the record.

## Still open

### A. 3D `ShapeProxy` construction — blocks 3D `overlapQuery`/`sweepQuery`

`33ecce4` promoted the **2D** `ShapeProxy` to a Storable record with a
`withShapeProxy` bracket (consumed: 2D `overlapQuery`/`sweepQuery`), but
the **3D** `b3ShapeProxy` is still an opaque tag. It differs from 2D:
`{ const b3Vec3* points; int count; float radius }` points into caller
memory, so it wants a hand-written bracket that owns the borrow:
`withShapeProxy :: VS.Vector Vec3 -> Float -> (Ptr ShapeProxy -> IO a) -> IO a`.
Unblocks the 3D twins of the 2D shape queries — the apecs side is then
a near-verbatim port of the 2D implementation (`geometryProxy` over the
3D `Geometry`, ~point cloud + radius per constructor).

### B. Bind `b2GetLengthUnitsPerMeter` / b3 equivalent (minor)

The mover-solver port that assumed the default 1.0 is gone (the engine
solver is called directly now), but length-unit scaling still leaks into
tuning docs (sleep thresholds, slop-derived tolerances) with no way to
query it. Trivial binding; low urgency.

### C. [engine-level] 3D world snapshot/restore parity

2D has `b2World_Snapshot`/`Restore`/`CreateFromSnapshot` (wrapped as
`snapshotWorld`/`restoreWorld` in apecs-box2d); Box3D has no equivalent,
so apecs-box3d cannot offer rewind. A box3d engine feature, not a
binding gap.

### D. `MeshData` material-count accessor (small, surfaced by compound work)

`compoundFromChildren` gives every `CompoundMesh` child exactly one
material slot and documents that multi-material meshes (e.g. `gridMesh`
with a material count above one) trip the engine's material-count
assertion — there is no Haskell-side accessor to query a built
`MeshData`'s material count to validate or adapt. A small getter (or a
count field surfaced by the creators) would turn that doc caveat into a
checked error or proper support.

## Landed in 33ecce4 and consumed (for the record)

1. **2D `ShapeProxy` Storable + `withShapeProxy`** → apecs 2D
   `overlapQuery`/`sweepQuery`. (3D half still open, see A.)
2. **`ContactData`/`Manifold`/`ManifoldPoint` Storable +
   `Contact.getData :: ContactId -> IO ContactData`** → manifold normal
   and world contact points on `Collision` (begin-touch).
3. **`Box3D.Geometry` mesh/height-field authoring**
   (`createMeshFromData`, `createHeightFieldFromData` + options records)
   → apecs `meshFromData` (incl. degenerate-triangle report) and
   `heightFieldFromData`.
4. **Compound authoring** (`CompoundCapsuleDef`/`HullDef`/`MeshDef`/
   `SphereDef`, `createCompoundFromData`) → apecs `Compound` handle,
   `CompoundChild`, `compoundFromChildren`, `GeoCompound`.
5. **`hs_b3Shape_TestPoint` shim** → 3D `containsPointQuery` now asks
   the engine; the analytic sphere/capsule dispatch is deleted.
6. **`CollisionPlane`/`PlaneSolverResult` Storable + `Box2D/Box3D.Mover`**
   → both wrappers' hand-ported `solvePlanes`/`clipVector` are deleted;
   `moveCharacter` drives the engine solver.
7. **Chain contact/hit-event toggles + `chainDefEnableSensorEvents`**
   → chain segments are stamped, registered and event/query-visible;
   chains opt into sensor visibility at creation.
